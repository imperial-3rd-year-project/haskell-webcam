{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Graphics.Capture.V4L2.Device (Device (..)) where

import Bindings.Linux.VideoDev2
import Bindings.LibV4L2

import Control.Concurrent (threadWaitRead, forkIO, ThreadId)
import Control.Monad (filterM, forM_, forever)
import qualified Data.HashTable.IO as H
import Data.List (isPrefixOf)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0)
import Data.Word (Word8, Word32)
import Foreign.C.Error (throwErrnoIfMinus1RetryMayBlock_)
import Foreign.C.Types (CULong)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, ptrToIntPtr, intPtrToPtr, IntPtr (..))
import Foreign.Storable
import Graphics.Capture.Class
import System.Directory (listDirectory, pathIsSymbolicLink)
import System.FilePath.Posix ((</>))
import System.Posix.Types (Fd (..))

data Device a where
  Unopened  :: FilePath       -> Device U
  Opened    :: Fd -> FilePath -> Device O
  Streaming :: Fd -> FilePath -> ThreadId -> Device S

deriving instance Show (Device a)

instance VideoCapture Device where
  deviceDescription (Unopened path)    = path
  deviceDescription (Opened _ path)    = path
  deviceDescription (Streaming _ path _) = path
    
  -- finds all /dev/video* which are not links and wraps them in Device type
  getDevices = do
    devs <- listDirectory deviceDir

    let relNameDevs  = filter (isPrefixOf videoDevPrefix) devs
        fullNameDevs = map (deviceDir </>) relNameDevs

    videoDevs <- filterM (fmap not . pathIsSymbolicLink) fullNameDevs
    return $ map Unopened videoDevs
    where
      deviceDir       = "/dev"
      videoDevPrefix  = "video"

  startCapture = startV4L2Capture


v4l2_ioctl :: Fd -> CULong -> Ptr a -> IO ()
v4l2_ioctl fd@(Fd fd') req p = throwErrnoIfMinus1RetryMayBlock_ "Graphics.Capture.V4L2.Device.v4l2_ioctl" ioctl onBlock
  where
    ioctl   = c'v4l2_ioctl fd' req p
    onBlock = threadWaitRead fd

startV4L2Capture :: Device O -> (Vector Word8 -> IO ()) -> IO (Device S)
startV4L2Capture (Opened fd path) f = do
  bufferSize <- setFormat
  reqBuffers

  buffers :: H.BasicHashTable (Ptr Word8) (ForeignPtr Word8) <- H.new

  -- We need to keep reference to enqueued buffers so they do not get deleted by the garbage collector
  enqueueBuffers bufferSize buffers

  streamThread <- forkIO $ do
    alloca $ \(typePtr :: Ptr C'v4l2_buf_type) -> do
      poke typePtr c'V4L2_BUF_TYPE_VIDEO_CAPTURE
      v4l2_ioctl fd c'VIDIOC_STREAMON typePtr
    forever (processFrame buffers)

  return $ Streaming fd path streamThread
  where
    numBuffers = 5

    setFormat :: IO Word32
    setFormat = alloca $ \(fmtPtr :: Ptr C'v4l2_format) -> do
      fillBytes fmtPtr 0 (sizeOf (undefined :: C'v4l2_format))
      v4l2_ioctl fd c'VIDIOC_G_FMT fmtPtr
      format <- c'v4l2_format'fmt <$> peek fmtPtr
      let pixelFormat = c'v4l2_format_u'pix format
      poke fmtPtr $ C'v4l2_format { c'v4l2_format'type = c'V4L2_BUF_TYPE_VIDEO_CAPTURE
                                  , c'v4l2_format'fmt  = format { c'v4l2_format_u'pix = pixelFormat { c'v4l2_pix_format'field       = c'V4L2_FIELD_INTERLACED
                                                                                                    , c'v4l2_pix_format'pixelformat = c'V4L2_PIX_FMT_RGB24
                                                                                                    }
                                                                }
                                  }
      v4l2_ioctl fd c'VIDIOC_S_FMT fmtPtr
      return $ c'v4l2_pix_format'sizeimage pixelFormat

    reqBuffers :: IO ()
    reqBuffers = alloca $ \(reqPtr :: Ptr C'v4l2_requestbuffers) -> do
      fillBytes reqPtr 0 (sizeOf (undefined :: C'v4l2_requestbuffers))
      req <- peek reqPtr
      poke reqPtr $ req { c'v4l2_requestbuffers'count    = numBuffers
                        , c'v4l2_requestbuffers'type     = c'V4L2_BUF_TYPE_VIDEO_CAPTURE
                        , c'v4l2_requestbuffers'memory   = c'V4L2_MEMORY_USERPTR
                        }

      v4l2_ioctl fd c'VIDIOC_REQBUFS reqPtr

    newBuffer :: Word32 -> H.BasicHashTable (Ptr Word8) (ForeignPtr Word8) -> IO (ForeignPtr Word8)
    newBuffer bufferSize buffers = do 
      buffer <- mallocForeignPtrBytes (fromIntegral bufferSize)
      H.insert buffers (unsafeForeignPtrToPtr buffer) buffer
      return buffer

    mkBuffer_u :: ForeignPtr Word8 -> C'v4l2_buffer_u
    mkBuffer_u buffer = C'v4l2_buffer_u { c'v4l2_buffer_u'offset  = 0
                                        , c'v4l2_buffer_u'userptr = let IntPtr ptr = ptrToIntPtr bufferPtr in fromIntegral ptr
                                        }
      where bufferPtr = unsafeForeignPtrToPtr buffer

    enqueueBuffers :: Word32 -> H.BasicHashTable (Ptr Word8) (ForeignPtr Word8) -> IO ()
    enqueueBuffers bufferSize buffers = forM_ [0..numBuffers - 1] $ \i -> alloca $ \(v4BufPtr :: Ptr C'v4l2_buffer) -> do
      buffer <- newBuffer bufferSize buffers

      fillBytes v4BufPtr 0 (sizeOf (undefined :: C'v4l2_buffer))
      v4Buf <- peek v4BufPtr
      poke v4BufPtr $ v4Buf { c'v4l2_buffer'index  = i
                            , c'v4l2_buffer'type   = c'V4L2_BUF_TYPE_VIDEO_CAPTURE
                            , c'v4l2_buffer'memory = c'V4L2_MEMORY_USERPTR
                            , c'v4l2_buffer'u      = mkBuffer_u buffer
                            , c'v4l2_buffer'length = bufferSize
                            }

      v4l2_ioctl fd c'VIDIOC_QBUF v4BufPtr

    processFrame :: H.BasicHashTable (Ptr Word8) (ForeignPtr Word8) -> IO ()
    processFrame buffers = do 
      threadWaitRead fd
      alloca $ \(v4BufPtr :: Ptr C'v4l2_buffer) -> do
        fillBytes v4BufPtr 0 (sizeOf (undefined :: C'v4l2_buffer))
        
        v4Buf <- peek v4BufPtr
        poke v4BufPtr v4Buf { c'v4l2_buffer'type   = c'V4L2_BUF_TYPE_VIDEO_CAPTURE
                            , c'v4l2_buffer'memory = c'V4L2_MEMORY_USERPTR
                            }

        v4l2_ioctl fd c'VIDIOC_DQBUF v4BufPtr

        dqV4Buf <- peek v4BufPtr

        let bufPtr :: Ptr Word8 = intPtrToPtr . fromIntegral . c'v4l2_buffer_u'userptr . c'v4l2_buffer'u $ dqV4Buf

        maybeForeignPtr <- buffers `H.lookup` bufPtr

        case maybeForeignPtr of
          Nothing -> error "buffer does not exist"
          -- Convert the buffer to a Vector, then send it to callback in a new thread
          Just buffer -> do
            let bytesUsed  = c'v4l2_buffer'bytesused dqV4Buf

            _ <- forkIO $ f (unsafeFromForeignPtr0 buffer (fromIntegral bytesUsed))

            buffers `H.delete` bufPtr

        -- enqueue a new buffer
        let bufferSize = c'v4l2_buffer'length dqV4Buf
        buffer' <- newBuffer bufferSize buffers
        poke v4BufPtr dqV4Buf { c'v4l2_buffer'u = mkBuffer_u buffer' }
        v4l2_ioctl fd c'VIDIOC_QBUF v4BufPtr
