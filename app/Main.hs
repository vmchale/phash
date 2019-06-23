module Main (main) where

import           Data.Foldable              (foldrM, toList)
import           Data.List                  (intercalate)
import           Data.List.NonEmpty         (NonEmpty (..), (<|))
import qualified Data.Map                   as M
import qualified Data.Set                   as S
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

displayHash :: NonEmpty FilePath -> Word64 -> String
displayHash fps h = show h ++ " " ++ intercalate ", " (toList fps)

filterDup :: M.Map Word64 (NonEmpty FilePath) -> M.Map Word64 (NonEmpty FilePath)
filterDup = M.filter p
    where p :: NonEmpty FilePath -> Bool
          p (_ :| (_:_)) = True
          p _            = False

displayAll :: M.Map Word64 (NonEmpty FilePath) -> String
displayAll hashes = intercalate "\n" (mkLine <$> M.toList hashes)
    where mkLine (h, fps) = displayHash fps h

-- TODO: benchmark this. In particular, verify filterWithKey is slower?
pruneBullshit :: M.Map Word64 (NonEmpty FilePath) -> M.Map Word64 (NonEmpty FilePath)
pruneBullshit = flip M.withoutKeys (S.singleton 0)

main :: IO ()
main = do
    images <- filter (imgExtension . takeExtension) <$> getDirRecursive "."
    putStrLn . displayAll =<< (filterDup . pruneBullshit <$> mkMap images)
