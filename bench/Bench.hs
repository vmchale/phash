module Main (main) where

import           Criterion.Main
import           Graphics.Image (VU (..), readImageY)
import           PerceptualHash (fileHash, fileHashPar, imgHash)

main :: IO ()
main =
    defaultMain [ env img $ \ f ->
                  bgroup "imgHash"
                      [ bench "cat.png" $ nf imgHash f ]
                , bgroup "fileHash"
                      [ bench "cat.png" $ nfIO (fileHash catPath) ]
                , bgroup "fileHashPar"
                      [ bench "cat.png" $ nfIO (fileHashPar catPath) ]
                ]
    where img = readImageY VU catPath
          catPath = "demo-data/cat.png"
