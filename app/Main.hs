module Main (main) where

import           Data.Foldable       (toList)
import           Data.List           (intercalate)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Word           (Word64)
import           Options.Applicative (execParser)
import           Parallel
import           Parser

displayImg :: Img -> String
displayImg (Img fp h w) = fp ++ " (" ++ show h ++ " × " ++ show w ++ ")"

displayPaths :: NonEmpty Img -> String
displayPaths = intercalate ", " . toList . fmap displayImg

displayHash :: NonEmpty Img -> Word64 -> String
displayHash fps h = show h ++ " " ++ displayPaths fps

{-# SCC filterDup #-}
filterDup :: [(Word64, NonEmpty a)] -> [(Word64, NonEmpty a)]
filterDup = filter (p.snd)
    where p :: NonEmpty a -> Bool
          p (_ :| (_:_)) = True
          p _            = False

displayDebug :: [(Word64, NonEmpty Img)] -> String
displayDebug hashes = intercalate "\n" (mkLine <$> hashes)
    where mkLine (h, fps) = displayHash fps h

displayAll :: [(a, NonEmpty Img)] -> String
displayAll fps = intercalate "\n" (displayPaths.snd <$> fps)

main :: IO ()
main = run =<< execParser wrapper

run :: ([FilePath], Bool, [FilePath]) -> IO ()
run (fps, debug, excls) = do
    let displayF = if debug
        then displayDebug
        else displayAll . filterDup
    putStrLn . displayF =<< pathMaps fps excls
