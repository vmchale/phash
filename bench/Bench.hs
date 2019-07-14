module Main (main) where

import           Criterion.Main
import           Graphics.Image (VU (..), readImageY)
import           PerceptualHash

main :: IO ()
main =
    defaultMain [ env img $ \ f ->
                  bgroup "imgHash"
                      [ bench "cat.png" $ nf imgHash f ]
                ]
    where img = readImageY VU "demo-data/cat.png"
