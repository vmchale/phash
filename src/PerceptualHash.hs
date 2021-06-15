{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module PerceptualHash ( imgHash
                      , fileHash
                      , hammingDistance
                      ) where

import qualified Codec.Picture                 as JuicyPixels
import           Codec.Picture.WebP            (decodeRgb8)
import           Control.Monad.ST              (runST)
import           Data.Bits                     (Bits, popCount, shiftL, xor, (.|.))
import qualified Data.ByteString               as BS
import           Data.List                     (isSuffixOf)
import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Storable          as VS
import           Data.Word                     (Word64, Word8)
import           Graphics.Image                (Array, Bilinear (..), Border (Edge, Reflect), Image,
                                                Pixel (PixelX, PixelY), RGB, RSU (..), VS, X, Y,
                                                convert, convolve, crop, makeImage, readImage,
                                                resize, transpose, (|*|))
import           Graphics.Image.Interface      (fromVector, toVector)
import qualified Graphics.Image.Interface      as Hip
import           Graphics.Image.Interface.Repa (fromRepaArrayS, toRepaArray)
import           Median                        (median)

-- | See
-- [wiki](https://en.wikipedia.org/wiki/Hamming_distance#Algorithm_example).
--
-- @since 0.1.4.0
{-# SPECIALIZE hammingDistance :: Word64 -> Word64 -> Int #-}
hammingDistance :: Bits a => a -> a -> Int
hammingDistance x y = popCount (x `xor` y)

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

fileWebp :: FilePath -> IO (Image VS RGB Word8)
fileWebp fp = do
    contents <- BS.readFile fp
    let (JuicyPixels.Image m n pixels) = decodeRgb8 contents
    pure $ fromVector (m, n) $ VS.unsafeCast pixels

readWebp :: FilePath -> IO (Image VS Y Double)
readWebp = fmap convert . fileWebp

-- | @since 0.1.5.0
fileHashWebp :: FilePath -> IO Word64
fileHashWebp = fmap (imgHash . convRepa) . readWebp
    -- faster
    where convRepa = fromRepaArrayS . toRepaArray

fileHash :: FilePath -> IO (Either String Word64)
fileHash fp | ".webp" `isSuffixOf` fp = pure <$> fileHashWebp fp
            | otherwise = fileHashHip fp

fileHashHip :: FilePath -> IO (Either String Word64)
fileHashHip = fmap (fmap (imgHash :: Image RSU Y Double -> Word64)) . readImage
