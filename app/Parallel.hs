module Parallel ( pathMaps ) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Control.Monad               (when)
import           Data.List.NonEmpty          (NonEmpty (..), (<|))
import qualified Data.Map                    as M
import           Data.Word                   (Word64)
import           PerceptualHash              (fileHash)
import           System.Directory.Parallel   (parTraverse)
import           System.FilePath             (takeExtension)

imgExtension :: String -> Bool
imgExtension ".jpg"  = True
imgExtension ".jpeg" = True
imgExtension ".png"  = True
imgExtension ".gif"  = True
imgExtension ".ppm"  = True
imgExtension ".hdr"  = True
imgExtension ".bmp"  = True
imgExtension ".TGA"  = True
imgExtension ".tif"  = True
imgExtension ".tiff" = True
imgExtension _       = False

insertHash :: FilePath -> IO (M.Map Word64 (NonEmpty FilePath) -> M.Map Word64 (NonEmpty FilePath))
insertHash fp = do
    hash <- fileHash fp
    pure $ \hashes ->
        case M.lookup hash hashes of
            Just others -> M.insert hash (fp <| others) hashes
            Nothing     -> M.insert hash (fp :| []) hashes

stepMap :: Bool -> TVar (M.Map Word64 (NonEmpty FilePath)) -> FilePath -> IO ()
stepMap dbg var fp = do
    when dbg $
        putStrLn fp
    mod' <- insertHash fp
    atomically $ modifyTVar' var mod'

pathMaps :: Bool -> [FilePath] -> IO (M.Map Word64 (NonEmpty FilePath))
pathMaps dbg fps = do
    total <- newTVarIO mempty
    parTraverse (stepMap dbg total) fileFilter (\_ -> pure True) fps
    readTVarIO total

    where fileFilter = pure . imgExtension . takeExtension
