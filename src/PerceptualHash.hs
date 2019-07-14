{-# LANGUAGE FlexibleContexts #-}

module PerceptualHash ( fileHash ) where

import           Control.Applicative          (pure)
import           Control.Monad.ST             (runST)
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import           Data.Word                    (Word64)
import           Foreign.C.String             (CString, withCString)
import           Foreign.C.Types              (CInt (..), CULLong)
import           Foreign.Marshal.Alloc        (alloca)
import           Foreign.Ptr                  (Ptr)
import           Foreign.Storable             (peek)
import           Graphics.Image
import           Graphics.Image.Interface     (toVector)
import           Graphics.Image.Processing

dct32 :: (Floating e, Array arr Y e) => Image arr Y e
dct32 = makeImage (32,32) gen
    where gen (i,j) = PixelY $ sqrt(2/n) * cos((fromIntegral ((2*i+1) * j) * pi)/(2*n))
          n = 32

meanFilter :: (Fractional e, Array arr X e, Array arr cs e) => Image arr cs e -> Image arr cs e
meanFilter = convolve Edge (fromLists convolveWith)
    where convolveWith = [[ if i == j then 1/7 else 0 | i <- [0..7]] | j <- [0..7]]

size32 :: Array arr cs e => Image arr cs e -> Image arr cs e
size32 = resize Bilinear Edge (32,32)

crop8 :: Array arr cs e => Image arr cs e -> Image arr cs e
crop8 = crop (0,0) (8,8)

medianImmut :: (Ord e, V.Unbox e, Fractional e) => V.Vector e -> e
medianImmut v = runST $ do
    v' <- V.thaw v
    Merge.sort v'
    let l = MV.length v'
    if odd l
        then v' `MV.read` ((l - 1) `div` 2)
        else do
            x0 <- v' `MV.read` ((l `div` 2) - 1)
            x1 <- v' `MV.read` (l `div` 2)
            pure $ (x0 + x1) / 2

fileHash :: FilePath -> IO Word64
fileHash fp = do
    grey <- readImageY VU fp
    let filtered = meanFilter grey
        preDct = size32 filtered
        postDct = dct32 |*| preDct |*| transpose dct32
        cropped = crop8 postDct
        flattened = V.map toDouble (toVector cropped)
        med = medianImmut flattened
        binVect = V.map (<med) flattened
    pure $ asWord64 binVect
    where toDouble :: Pixel Y Double -> Double
          toDouble (PixelY x) = x
          asWord64 :: V.Vector Bool -> Word64
          asWord64 = V.foldl' (\acc x -> acc * 2 + boolToWord64 x) 0
          boolToWord64 :: Bool -> Word64
          boolToWord64 False = 0
          boolToWord64 True  = 1
