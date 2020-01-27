{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module PerceptualHash ( imgHash
                      , fileHash
                      ) where

import           Control.Monad.ST         (runST)
import           Data.Bits                (shiftL, (.|.))
import qualified Data.Vector.Generic      as V
import           Data.Word                (Word64)
import           Graphics.Image           (Array, Bilinear (..), Border (Edge, Reflect), Image,
                                           Pixel (PixelX, PixelY), RSU (..), X, Y, convolve, crop,
                                           makeImage, readImage, resize, transpose, (|*|))
import           Graphics.Image.Interface (toVector)
import qualified Graphics.Image.Interface as Hip
import           Median                   (median)

dct32 :: (Floating e, Array arr Y e) => Image arr Y e
dct32 = makeImage (32,32) gen
    where gen (i,j) = PixelY $ sqrt(2/n) * cos((fromIntegral ((2*i) * (j-1)) * pi)/(2*n))
          n = 32

idMat :: (Fractional e, Array arr X e) => Image arr X e
idMat = makeImage (7,7) (\_ -> PixelX (1/49))

{-# INLINE meanFilter #-}
meanFilter :: (Fractional e, Array arr X e, Array arr cs e) => Image arr cs e -> Image arr cs e
meanFilter = {-# SCC "meanFilter" #-} convolve Reflect idMat

{-# INLINE size32 #-}
size32 :: Array arr cs e => Image arr cs e -> Image arr cs e
size32 = resize Bilinear Edge (32,32)

crop8 :: Array arr cs e => Image arr cs e -> Image arr cs e
crop8 = crop (0,0) (8,8)

medianImmut :: (Ord e, Fractional e, V.Vector v e) => v e -> e
medianImmut v = runST $
    median =<< V.thaw v

dct :: (Floating e, Array arr Y e) => Image arr Y e -> Image arr Y e
dct img = dct32 |*| img |*| transpose dct32

{-# INLINE imgHash #-}
-- | DCT based hash. See
-- [Zauner](https://www.phash.org/docs/pubs/thesis_zauner.pdf).
--
-- It is suggested that you use this with the Repa backend.
imgHash :: (Ord e, Floating e, Array arr Y e, Array arr X e, V.Vector (Hip.Vector arr) Bool, V.Vector (Hip.Vector arr) e) => Image arr Y e -> Word64
imgHash = asWord64 . aboveMed . V.map (\(PixelY x) -> x) . toVector . crop8 . dct . size32 . meanFilter

asWord64 :: V.Vector v Bool => v Bool -> Word64
asWord64 = V.foldl' (\acc x -> (acc `shiftL` 1) .|. boolToWord64 x) 0
    where boolToWord64 :: Bool -> Word64
          boolToWord64 False = 0
          boolToWord64 True  = 1

aboveMed :: (Fractional e, V.Vector v e, V.Vector v Bool, Ord e) => v e -> v Bool
aboveMed v =
    let med = medianImmut v
    in V.map (<med) v

fileHash :: FilePath -> IO (Either String Word64)
fileHash = fmap (fmap (imgHash :: Image RSU Y Double -> Word64)) . readImage
