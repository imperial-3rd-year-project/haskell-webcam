module ConversionUtilsSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (resize)

import Data.Word (Word8)
import Graphics.Display.ConversionUtils 

import qualified Data.Vector.Storable as V

dims :: (Int, Int)
dims = (640, 480)

pixelSize :: Int
pixelSize = 3

vector1 :: V.Vector Word8
vector1 = V.fromList [ fromIntegral (i `mod` 2) | i <- [0..fst dims - 1], _ <- [0..snd dims - 1], _ <- [0..pixelSize - 1]]

pictureSize (x, y) = x * y * pixelSize


spec :: Spec
spec = do 
  describe "image resize" $ do
    it "test vector has the appropriate size" $ property $ 
        V.length vector1 == pictureSize dims

    it "returns the same image for same dimensions" $ property $ 
        let offset = centredOffset dims dims in 
        resize offset dims dims vector1 == vector1

    it "length of resized vector matches downscaled dimensions" $ property $
        let newDims   = (600, 300)
            offset    = centredOffset dims newDims
            newVector = resize offset dims newDims vector1 in 
        V.length newVector == pictureSize newDims

    it "length of resized vector matches upscaled dimensions" $ property $
        let newDims   = (700, 600)
            offset    = centredOffset dims newDims
            newVector = resize offset dims newDims vector1 in 
        V.length newVector == pictureSize newDims
    
    it "upscaling followed by downscaling returns same image" $ property $
        let newDims   = (700, 600)
            offset    = centredOffset dims newDims
            offset'   = centredOffset newDims dims
            newVector = resize offset dims newDims vector1 in 
        resize offset' newDims dims newVector == vector1

    it "height upscaled version contains same picture" $ property $
        let newDims   = (640, 482)
            offset    = centredOffset dims newDims
            newVector = resize offset dims newDims vector1 in 
        V.slice (fst newDims * pixelSize) (pictureSize dims) newVector == vector1

    it "height downscaled version contains same picture" $ property $
        let newDims   = (640, 478)
            offset    = centredOffset dims newDims
            newVector = resize offset dims newDims vector1 in 
        V.slice (fst dims * pixelSize) (pictureSize newDims) vector1 == newVector

    it "width upscaled version contains same picture" $ property $
        let newDims   = (642, 480)
            offset    = centredOffset dims newDims
            newVector = resize offset dims newDims vector1 in 
        [V.slice ((i * fst newDims + 1) * pixelSize) (fst dims) newVector | i <- [0..snd newDims - 1]]
          == [V.slice (i * fst dims * pixelSize) (fst dims) vector1 | i <- [0..snd dims - 1]]

    it "width downscaled version contains same picture" $ property $
        let newDims   = (638, 480)
            offset    = centredOffset dims newDims
            newVector = resize offset dims newDims vector1 in 
        [V.slice ((i * fst dims + 1) * pixelSize) (fst newDims) vector1 | i <- [0..snd dims - 1]]
          == [V.slice (i * fst newDims * pixelSize) (fst newDims) newVector | i <- [0..snd newDims - 1]]

    it "offset (0, 0) places image in the top left corner" $ property $
        let newDims   = (700, 600)
            offset    = (0, 0)
            newVector = resize offset dims newDims vector1 in
        [V.slice ((i * fst dims) * pixelSize) (fst dims) vector1 | i <- [0..snd dims - 1]]
          == [V.slice (i * fst newDims * pixelSize) (fst dims) newVector | i <- [0..snd dims - 1]]
