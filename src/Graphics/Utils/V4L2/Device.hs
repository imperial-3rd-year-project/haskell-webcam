module Graphics.Utils.V4L2.Device where

import Bindings.LibV4L2
import Control.Concurrent (threadWaitRead)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1RetryMayBlock_)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt, CULong)
import Foreign.Ptr (Ptr)
import System.Posix.Types (Fd(Fd))

v4l2_open :: FilePath -> CInt -> String -> IO Fd
v4l2_open path flags err = withCString path $ \p -> do
    fd <- throwErrnoIfMinus1 err (c'v4l2_open p flags 0)
    return (fromIntegral fd)


v4l2_ioctl :: Fd -> CULong -> Ptr a -> String -> IO ()
v4l2_ioctl fd@(Fd fd') req p err = throwErrnoIfMinus1RetryMayBlock_ err ioctl onBlock
  where
    ioctl   = c'v4l2_ioctl fd' req p
    onBlock = threadWaitRead fd
