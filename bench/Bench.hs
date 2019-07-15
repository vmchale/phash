module Main (main) where

import           Criterion.Main
import           Graphics.Image (VU (..), readImageY)
import           PerceptualHash (imgHash)

main :: IO ()
main =
    defaultMain [ env img $ \ f ->
                  bgroup "imgHash"
                      [ bench "cat.png" $ nf imgHash f ]
                ]
    where img = readImageY VU catPath
          catPath = "demo-data/cat.png"
