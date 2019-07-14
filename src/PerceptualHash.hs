{-# LANGUAGE FlexibleContexts #-}

module PerceptualHash ( imgHash
                      , fileHash
                      ) where

import           Control.Applicative      (pure)
import           Control.Monad.ST         (runST)
import qualified Data.Vector.Unboxed      as V
import           Data.Word                (Word64)
import           Graphics.Image
import           Graphics.Image.Interface (toVector)
import           Median

dct32 :: (Floating e, Array arr Y e) => Image arr Y e
dct32 = makeImage (32,32) gen
    where gen (i,j) = PixelY $ sqrt(2/n) * cos((fromIntegral ((2*i+1) * j) * pi)/(2*n))
          n = 32

meanFilter :: (Fractional e, Array arr X e, Array arr cs e) => Image arr cs e -> Image arr cs e
meanFilter = convolve Edge (fromLists convolveWith)
    where convolveWith = [[ if i == j then 1/7 else 0 | i <- bounds] | j <- bounds]
          bounds :: [Int]
          bounds = [0..7]

size32 :: Array arr cs e => Image arr cs e -> Image arr cs e
size32 = resize Bilinear Edge (32,32)

crop8 :: Array arr cs e => Image arr cs e -> Image arr cs e
crop8 = crop (0,0) (8,8)

-- TODO: check this actually works
medianImmut :: (Ord e, V.Unbox e, Fractional e) => V.Vector e -> e
medianImmut v = runST $
    median =<< V.thaw v

dct :: (Floating e, Array arr Y e) => Image arr Y e -> Image arr Y e
dct img = dct32 |*| img |*| transpose dct32

imgHash :: Image VU Y Double -> Word64
imgHash = asWord64 . aboveMed . V.map d . toVector . crop8 . dct . size32 . meanFilter
    where d :: Pixel Y Double -> Double
          d (PixelY x) = x
          asWord64 :: V.Vector Bool -> Word64
          asWord64 = V.foldl' (\acc x -> acc * 2 + boolToWord64 x) 0
          boolToWord64 :: Bool -> Word64
          boolToWord64 False = 0
          boolToWord64 True  = 1
          aboveMed v =
            let med = medianImmut v
            in V.map (<med) v

fileHash :: FilePath -> IO Word64
fileHash fp = do
    grey <- readImageY VU fp
    pure $ imgHash grey
