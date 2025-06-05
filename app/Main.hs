module Main (main) where

import           Data.Foldable       (toList)
import           Data.List           (intercalate)
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.Map            as M
import           Data.Word           (Word64)
import           Options.Applicative (execParser)
import           Parallel
import           Parser

displayPaths :: NonEmpty (FilePath, (Int, Int)) -> String
displayPaths = intercalate ", " . toList . fmap (\(fp, (x,y)) -> fp ++ "(" ++ show x ++ "x" ++ show y ++ ")")

displayHash :: NonEmpty (FilePath, (Int, Int)) -> Word64 -> String
displayHash fps h = show h ++ " " ++ displayPaths fps

filterDup :: M.Map Word64 (NonEmpty (FilePath, (Int, Int))) -> M.Map Word64 (NonEmpty (FilePath, (Int, Int)))
filterDup = M.filter p
    where p :: NonEmpty (FilePath, (Int, Int)) -> Bool
          p (_ :| (_:_)) = True
          p _            = False

displayDebug :: M.Map Word64 (NonEmpty (FilePath, (Int, Int))) -> String
displayDebug hashes = intercalate "\n" (mkLine <$> M.toList hashes)
    where mkLine (h, fps) = displayHash fps h

displayAll :: M.Map a (NonEmpty (FilePath, (Int, Int))) -> String
displayAll fps = intercalate "\n" (displayPaths <$> toList fps)

main :: IO ()
main = run =<< execParser wrapper

run :: ([FilePath], Bool, [FilePath]) -> IO ()
run (fps, debug, excls) = do
    let displayF = if debug
        then displayDebug
        else displayAll . filterDup
    putStrLn . displayF =<< pathMaps fps excls
