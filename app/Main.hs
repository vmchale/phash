module Main (main) where

import           Data.Foldable       (toList)
import           Data.List           (intercalate)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Word           (Word64)
import           Options.Applicative (execParser)
import           Parallel
import           Parser
import qualified Word.Map            as M

displayPaths :: NonEmpty FilePath -> String
displayPaths = intercalate ", " . toList

displayHash :: NonEmpty FilePath -> Word64 -> String
displayHash fps h = show h ++ " " ++ displayPaths fps

filterDup :: M.Map (NonEmpty FilePath) -> M.Map (NonEmpty FilePath)
filterDup = M.filter p
    where p :: NonEmpty FilePath -> Bool
          p (_ :| (_:_)) = True
          p _            = False

displayDebug :: M.Map (NonEmpty FilePath) -> String
displayDebug hashes = intercalate "\n" (mkLine <$> M.toList hashes)
    where mkLine (h, fps) = displayHash fps h

displayAll :: M.Map (NonEmpty FilePath) -> String
displayAll fps = intercalate "\n" (displayPaths <$> toList fps)

main :: IO ()
main = run =<< execParser wrapper

run :: ([FilePath], Bool) -> IO ()
run (fps, debug) = do
    let displayF = if debug
        then displayDebug
        else displayAll . filterDup
    putStrLn . displayF =<< pathMaps fps
