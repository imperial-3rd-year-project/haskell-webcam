module Graphics.Display.ConversionUtils where

import Data.Vector.Storable as V

import Data.Word (Word8)
import Codec.Picture (Image(Image), PixelRGB8)

-- Given image, its width, and height returns JuicyPixelImage
toJuicyPixelImage :: (Int, Int) -> V.Vector Word8 -> Image PixelRGB8
toJuicyPixelImage = uncurry Image
