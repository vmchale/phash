module Main (main) where

import           PerceptualHash             (fileHash)
import           System.Directory.Recursive
import           System.FilePath            (takeExtension)

imgExtension :: String -> Bool
imgExtension ".jpg"  = True
imgExtension ".jpeg" = True
imgExtension ".png"  = True
imgExtension ".gif"  = True
imgExtension _       = False -- idgaf about TIFF

main :: IO ()
main = do
    images <- filter (imgExtension . takeExtension) <$> getDirRecursive "."
    print =<< traverse fileHash images
