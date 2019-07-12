module Main (main) where

import           Data.Foldable       (toList)
import           Data.List           (intercalate)
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Word           (Word64)
import           Options.Applicative (execParser)
import           Parallel
import           Parser

displayPaths :: NonEmpty FilePath -> String
displayPaths = intercalate ", " . toList

displayHash :: NonEmpty FilePath -> Word64 -> String
displayHash fps h = show h ++ " " ++ displayPaths fps

filterDup :: M.Map Word64 (NonEmpty FilePath) -> M.Map Word64 (NonEmpty FilePath)
filterDup = M.filter p
    where p :: NonEmpty FilePath -> Bool
          p (_ :| (_:_)) = True
          p _            = False

displayDebug :: M.Map Word64 (NonEmpty FilePath) -> String
displayDebug hashes = intercalate "\n" (mkLine <$> M.toList hashes)
    where mkLine (h, fps) = displayHash fps h

displayAll :: M.Map a (NonEmpty FilePath) -> String
displayAll fps = intercalate "\n" (displayPaths <$> toList fps)

-- TODO: benchmark this. In particular, verify filterWithKey is slower?
pruneBullshit :: M.Map Word64 (NonEmpty FilePath) -> M.Map Word64 (NonEmpty FilePath)
pruneBullshit = flip M.withoutKeys (S.singleton 0)

main :: IO ()
main = run =<< execParser wrapper

run :: ([FilePath], Bool) -> IO ()
run (fps, debug) = do
    let displayF = if debug
        then displayDebug . pruneBullshit
        else displayAll . filterDup . pruneBullshit
    putStrLn . displayF =<< pathMaps fps
