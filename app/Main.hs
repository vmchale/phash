module Main (main) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO,
                                              readTVarIO)
import           Data.Foldable               (toList)
import           Data.List                   (intercalate)
import           Data.List.NonEmpty          (NonEmpty (..), (<|))
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import           Data.Word                   (Word64)
import           Options.Applicative         (execParser)
import           Parser
import           PerceptualHash              (fileHash)
import           System.Directory.Parallel
import           System.FilePath             (takeExtension)

imgExtension :: String -> Bool
imgExtension ".jpg"  = True
imgExtension ".jpeg" = True
imgExtension ".png"  = True
imgExtension _       = False -- gif doesn't work with CImg AFAICT

insertHash :: FilePath -> IO (M.Map Word64 (NonEmpty FilePath) -> M.Map Word64 (NonEmpty FilePath))
insertHash fp = do
    hash <- fileHash fp
    pure $ \hashes ->
        case M.lookup hash hashes of
            Just others -> M.insert hash (fp <| others) hashes
            Nothing     -> M.insert hash (fp :| []) hashes

stepMap :: TVar (M.Map Word64 (NonEmpty FilePath)) -> FilePath -> IO ()
stepMap var fp = do
    mod' <- insertHash fp
    atomically $ modifyTVar' var mod'

pathMaps :: [FilePath] -> IO (M.Map Word64 (NonEmpty FilePath))
pathMaps fps = do
    total <- newTVarIO mempty
    parTraverse (stepMap total) fileFilter fps
    readTVarIO total

    where fileFilter = pure . imgExtension . takeExtension

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
