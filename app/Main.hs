module Main (main) where

import           Data.Foldable              (foldrM)
import           Data.List.NonEmpty         (NonEmpty (..), (<|))
import qualified Data.Map                   as M
import           Data.Word                  (Word64)
import           PerceptualHash             (fileHash)
import           System.Directory.Recursive
import           System.FilePath            (takeExtension)

imgExtension :: String -> Bool
imgExtension ".jpg"  = True
imgExtension ".jpeg" = True
imgExtension ".png"  = True
imgExtension _       = False -- gif doesn't work with CImg AFAICT

insertHash :: FilePath -> M.Map Word64 (NonEmpty FilePath) -> IO (M.Map Word64 (NonEmpty FilePath))
insertHash fp hashes = do
    hash <- fileHash fp
    case M.lookup hash hashes of
        Just others -> pure $ M.insert hash (fp <| others) hashes
        Nothing     -> pure $ M.insert hash (fp :| []) hashes

mkMap :: [FilePath] -> IO (M.Map Word64 (NonEmpty FilePath))
mkMap = foldrM insertHash mempty

main :: IO ()
main = do
    images <- filter (imgExtension . takeExtension) <$> getDirRecursive "."
    print =<< mkMap images
