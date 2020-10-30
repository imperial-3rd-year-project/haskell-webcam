module Graphics.Display.ConversionUtils (toJuicyPixelImage, toYUV420Image) where

import Data.Word (Word8)
import Codec.Picture (Image(Image), PixelRGB8)

import qualified Data.Vector.Storable as V

-- Given image, its width, and height returns JuicyPixelImage
toJuicyPixelImage :: (Int, Int) -> V.Vector Word8 -> Image PixelRGB8
toJuicyPixelImage = uncurry Image

-- TODO move this to general utils
toYUV420Image :: (Int, Int) -> V.Vector Word8 -> V.Vector Word8
toYUV420Image (w, h) img
  = V.fromList $ 
          ys 
          -- replicate 230400 0
          ++ dus 
          -- ++ replicate 57600 130
          ++ dvs 
          -- ++ replicate 57600 0
  where 
    (dus, dvs) = makeUVIter 0 0
    ys         = makeYIter 0

    take3 :: Int -> (Word8, Word8, Word8)
    take3 i = (rgb V.! 0, rgb V.! 1, rgb V.! 2)
      where
        rgb = V.slice i 3 img
    
    pix4avg :: Int -> Int -> (Word8, Word8, Word8)
    pix4avg i j = ( sum [r1, r2, r3, r4] `div` n
                  , sum [g1, g2, g3, g4] `div` n
                  , sum [b1, b2, b3, b4] `div` n)
      where
        n = 4
        (r1, g1, b1) = take3 (3 * (2 * i * w + 2 * j))
        (r2, g2, b2) = take3 (3 * (2 * i * w + 2 * j + 1))
        (r3, g3, b3) = take3 (3 * ((2 * i + 1) * w + 2 * j))
        (r4, g4, b4) = take3 (3 * ((2 * i + 1) * w + 2 * j + 1))

    makeYIter :: Int -> [Word8]
    makeYIter i
      | i >= w * h = []
      | otherwise  = rgbToY (take3 (3 * i)) : makeYIter (i + 1)
     
    makeUVIter :: Int -> Int -> ([Word8], [Word8])
    makeUVIter i j 
      | i >= h `div` 2 = ([], [])
      | j >= w `div` 2 = makeUVIter (i + 1) 0
      | otherwise      = (u:us, v:vs)
      where
        (u, v)   = rgbToUV (pix4avg i j)
        (us, vs) = makeUVIter i (j + 1)

rgbToY :: (Word8, Word8, Word8) -> Word8
rgbToY (r, g, b) 
    = round res
    where
      res = (wr * fromIntegral r + wg * fromIntegral g + wb * fromIntegral b + c)
      wr = 0.299 :: Double
      wg = 0.587 :: Double
      wb = 0.114 :: Double
      c  = 0

rgbToUV :: (Word8, Word8, Word8) -> (Word8, Word8)
rgbToUV (r, g, b) 
    = (round resu, round resv)
    where
      resu = wru * fromIntegral r + wgu * fromIntegral g + wbu * fromIntegral b + c 
      resv = wrv * fromIntegral r + wgv * fromIntegral g + wbv * fromIntegral b + c
      wru = -0.169 :: Double
      wgu = -0.331 :: Double
      wbu =  0.500 :: Double
      wrv =  0.500 :: Double
      wgv = -0.419 :: Double
      wbv = -0.081 :: Double
      c   = 128
