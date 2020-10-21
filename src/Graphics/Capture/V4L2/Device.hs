{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Graphics.Capture.V4L2.Device (Device (..)) where

import Bindings.LibV4L2
import Bindings.Linux.VideoDev2
import Bindings.Posix.Sys.Mman
import Bindings.Posix.Fcntl (c'O_RDWR, c'O_NONBLOCK)

import Control.Concurrent (threadWaitRead, forkIO, forkFinally, killThread, ThreadId)
import qualified Control.Concurrent.QSem as Sem
import Control.Monad (filterM, forM, forM_, forever)
import Data.Bits ((.|.))
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0)
import Data.Word (Word8, Word32)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1RetryMayBlock_)
import Foreign.C.String (withCString)
import Foreign.C.Types (CULong, CSize)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable
import GHC.Conc (closeFdWith)
import Graphics.Capture.Class
import System.Directory (listDirectory, pathIsSymbolicLink)
import System.FilePath.Posix ((</>))
import System.Posix.Types (Fd (..))

data Device a where
  Unopened  :: FilePath       -> Device U
  Opened    :: Fd -> FilePath -> Device O
  Streaming :: Fd -> FilePath -> ThreadId -> Sem.QSem -> Device S

instance Show (Device a) where
  show = deviceDescription

type Buffer = (Ptr Word8, CSize)

instance VideoCapture Device where
  deviceDescription (Unopened path)    = path
  deviceDescription (Opened _ path)    = path
  deviceDescription (Streaming _ path _ _) = path
    
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

  openDevice (Unopened path) = withCString path $ \p -> do
    fd <- throwErrnoIfMinus1 errorString (c'v4l2_open p (c'O_RDWR .|. c'O_NONBLOCK) 0)
    return $ Opened (fromIntegral fd) path
    where  
      errorString = "Graphics.Capture.V4L2.Device.openDevice"


  closeDevice (Opened fd path) = 
    closeFdWith closeFn fd >> (return $ Unopened path)
    where
       errorString = "Graphics.Capture.V4L2.Device.closeDevice"
       closeFn :: Fd -> IO ()
       closeFn fileDesc = 
         throwErrnoIfMinus1 errorString (c'v4l2_close (fromIntegral fileDesc)) >> return ()

  startCapture = startV4L2Capture
  stopCapture  = stopV4L2Capture


v4l2_ioctl :: Fd -> CULong -> Ptr a -> String -> IO ()
v4l2_ioctl fd@(Fd fd') req p err = putStrLn ("ioctl :" ++ err) >> throwErrnoIfMinus1RetryMayBlock_ err ioctl onBlock
  where
    ioctl   = c'v4l2_ioctl fd' req p
    onBlock = threadWaitRead fd

startV4L2Capture :: Device O -> (Vector Word8 -> IO ()) -> IO (Device S)
startV4L2Capture (Opened fd path) f = 
  let numBuffers = 2
   in do

  putStrLn "Setting formats"

  bufferSize <- setFormat

  putStrLn $ "Buffer size: " ++ show bufferSize


  numBuffersAllocated <- reqBuffers numBuffers

  putStrLn $ "Buffers requested; allocated " ++ show numBuffersAllocated

  buffers <- createBuffers numBuffers

  -- We need to keep reference to enqueued buffers so they do not get deleted by the garbage collector
  enqueueBuffersMMAP numBuffersAllocated

  putStrLn "Start capture"

  stopCaptureSem <- Sem.newQSem 0

  -- Thread turns off stream itself when terminates
  streamThread <- flip forkFinally (const (streamOff buffers stopCaptureSem)) $ do
    alloca $ \(typePtr :: Ptr C'v4l2_buf_type) -> do
      poke typePtr c'V4L2_BUF_TYPE_VIDEO_CAPTURE
      v4l2_ioctl fd c'VIDIOC_STREAMON typePtr "Graphics.Capture.V4L2.Device.startV4L2Capture: STREAMON"
    forever (processFrame buffers)

  return $ Streaming fd path streamThread stopCaptureSem
  where
    setFormat :: IO Word32
    setFormat = alloca $ \(fmtPtr :: Ptr C'v4l2_format) -> do
      fillBytes fmtPtr 0 (sizeOf (undefined :: C'v4l2_format))
      -- v4l2_ioctl fd c'VIDIOC_G_FMT fmtPtr
      putStrLn "Format retrived"
      format <- c'v4l2_format'fmt <$> peek fmtPtr

      let pixelFormat = (c'v4l2_format_u'pix format) { c'v4l2_pix_format'field       = c'V4L2_FIELD_INTERLACED
                                                     , c'v4l2_pix_format'pixelformat = c'V4L2_PIX_FMT_RGB24
                                                     , c'v4l2_pix_format'width = 640
                                                     , c'v4l2_pix_format'height = 480
                                                     }

      poke fmtPtr $ C'v4l2_format { c'v4l2_format'type = c'V4L2_BUF_TYPE_VIDEO_CAPTURE
                                  , c'v4l2_format'fmt  = format { c'v4l2_format_u'pix = pixelFormat }
                                  }

      pokeByteOff fmtPtr 8 pixelFormat

      v4l2_ioctl fd c'VIDIOC_S_FMT fmtPtr "Graphics.Capture.V4L2.Device.startV4L2Capture: S_FMT"
      filledFmt <- c'v4l2_format'fmt <$> peek fmtPtr
      let filledPixelFormat = c'v4l2_format_u'pix filledFmt
      return $ c'v4l2_pix_format'sizeimage filledPixelFormat

    reqBuffers :: Word32 -> IO Word32
    reqBuffers buffersRequested = alloca $ \(reqPtr :: Ptr C'v4l2_requestbuffers) -> do
      fillBytes reqPtr 0 (sizeOf (undefined :: C'v4l2_requestbuffers))
      req <- peek reqPtr
      poke reqPtr $ req { c'v4l2_requestbuffers'count    = buffersRequested
                        , c'v4l2_requestbuffers'type     = c'V4L2_BUF_TYPE_VIDEO_CAPTURE
                        , c'v4l2_requestbuffers'memory   = c'V4L2_MEMORY_MMAP
                        }

      v4l2_ioctl fd c'VIDIOC_REQBUFS reqPtr "Graphics.Capture.V4L2.Device.startV4L2Capture: REQBUFS"

      c'v4l2_requestbuffers'count <$> peek reqPtr

    mkPtr :: forall a b. Storable a => (Ptr a -> IO b) -> IO b
    mkPtr g = alloca $ \ptr -> fillBytes ptr 0 (sizeOf (undefined :: a)) >> g ptr

    createBuffers :: Word32 -> IO [Buffer]
    createBuffers buffersCount = forM [0..buffersCount - 1] $ \i -> mkPtr $ \(v4BufPtr :: Ptr C'v4l2_buffer) -> do 
      buffer <- peek v4BufPtr
      poke v4BufPtr buffer { c'v4l2_buffer'type = c'V4L2_BUF_TYPE_VIDEO_CAPTURE
                           , c'v4l2_buffer'memory = c'V4L2_MEMORY_MMAP
                           , c'v4l2_buffer'index = i
                           }
      v4l2_ioctl fd c'VIDIOC_QUERYBUF v4BufPtr "Graphics.Capture.V4L2.Device.startV4L2Capture: QUERYBUF"

      filledBuffer <- peek v4BufPtr

      let bufferLength :: CSize = fromIntegral . c'v4l2_buffer'length $ filledBuffer
          bufferOffset :: Int64 = fromIntegral . c'v4l2_buffer_u'offset . c'v4l2_buffer'u $ filledBuffer
          Fd fd'       = fd

      buf <- c'v4l2_mmap nullPtr bufferLength (c'PROT_READ .|. c'PROT_WRITE) c'MAP_SHARED fd' (fromIntegral bufferOffset)

      return (buf, bufferLength)

    enqueueBuffersMMAP :: Word32 -> IO ()
    enqueueBuffersMMAP buffersCount = forM_ [0..buffersCount - 1] $ \i -> mkPtr $ \(v4BufPtr :: Ptr C'v4l2_buffer) -> do
      v4Buf <- peek v4BufPtr
      poke v4BufPtr $ v4Buf { c'v4l2_buffer'type = c'V4L2_BUF_TYPE_VIDEO_CAPTURE
                            , c'v4l2_buffer'memory = c'V4L2_MEMORY_MMAP
                            , c'v4l2_buffer'index = i
                            }
      v4l2_ioctl fd c'VIDIOC_QBUF v4BufPtr "Graphics.Capture.V4L2.Device.startV4L2Capture: QBUF"

    processFrame :: [Buffer] -> IO ()
    processFrame buffers = do 
      threadWaitRead fd
      alloca $ \(v4BufPtr :: Ptr C'v4l2_buffer) -> do
        fillBytes v4BufPtr 0 (sizeOf (undefined :: C'v4l2_buffer))
        
        v4Buf <- peek v4BufPtr
        poke v4BufPtr v4Buf { c'v4l2_buffer'type   = c'V4L2_BUF_TYPE_VIDEO_CAPTURE
                            , c'v4l2_buffer'memory = c'V4L2_MEMORY_MMAP
                            }

        v4l2_ioctl fd c'VIDIOC_DQBUF v4BufPtr "Graphics.Capture.V4L2.Device.startV4L2Capture: DQBUF"

        dqV4Buf <- peek v4BufPtr

        let bufIndex :: Int= fromIntegral . c'v4l2_buffer'index $ dqV4Buf
            bytesUsed :: Int = fromIntegral . c'v4l2_buffer'bytesused $ dqV4Buf
            time = c'v4l2_buffer'timestamp $ dqV4Buf
            sec :: Int = fromIntegral . c'timeval'tv_sec $ time

        copyBuffer <- mallocForeignPtrBytes (fromIntegral bytesUsed)

        putStrLn $ show sec

        withForeignPtr copyBuffer $ \dst -> copyBytes dst (fst (buffers !! bufIndex)) bytesUsed

        _ <- forkIO (f (unsafeFromForeignPtr0 copyBuffer bytesUsed))

        v4l2_ioctl fd c'VIDIOC_QBUF v4BufPtr "Graphics.Capture.V4L2.Device.startV4L2Capture: QBUF"

    streamOff :: [Buffer] -> Sem.QSem -> IO ()
    streamOff buffers closeSem = do
      alloca $ \(typePtr :: Ptr C'v4l2_buf_type) -> do
        poke typePtr c'V4L2_BUF_TYPE_VIDEO_CAPTURE
        v4l2_ioctl fd c'VIDIOC_STREAMOFF typePtr "Graphics.Capture.V4L2.Device.streamOff: STREAMOFF"

      forM_ buffers $ \(bufPtr, bufSize) -> do
        c'v4l2_munmap bufPtr bufSize

      -- remove all buffers from kernel memory
      _ <- reqBuffers 0

      Sem.signalQSem closeSem

stopV4L2Capture :: Device S -> IO (Device O)
stopV4L2Capture (Streaming fd path streamThread closeSem) = killThread streamThread >> Sem.waitQSem closeSem >> return (Opened fd path)
