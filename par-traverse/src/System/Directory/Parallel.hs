module System.Directory.Parallel ( parTraverse
                                 , parTraverseAll
                                 ) where

import           Control.Concurrent                  (getNumCapabilities)
import           Control.Concurrent.ParallelIO.Local (Pool, parallel_, withPool)
import           Control.Monad                       (filterM)
import           System.Directory                    (doesDirectoryExist, listDirectory)
import           System.FilePath                     ((</>))

-- TODO: mapConcurrently?

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    if res
        then pure (x:as, bs)
        else pure (as, x:bs)

-- | @since 0.2.1.0
parTraverseAll :: (FilePath -> IO ()) -- ^ Action to execute on files
               -> (FilePath -> IO Bool) -- ^ Filter on files
               -> (FilePath -> IO Bool) -- ^ Filter on directories
               -> [FilePath] -- ^ Starting files/directories
               -> IO ()
parTraverseAll act fileP dirP fps = do
    (dirs, files) <- partitionM doesDirectoryExist fps
    parTraverseFiles act fileP dirP dirs files


parTraverseFiles :: (FilePath -> IO ()) -- ^ Action to execute on files
                 -> (FilePath -> IO Bool) -- ^ Filter on files
                 -> (FilePath -> IO Bool) -- ^ Filter on directories
                 -> [FilePath] -- ^ Starting directories
                 -> [FilePath] -- ^ Starting files (won't be filtered)
                 -> IO ()
parTraverseFiles act fileP dirP dirs files = do
    ncpu <- getNumCapabilities
    withPool ncpu $ \pool ->
        parallel_ pool $ fmap act files ++ fmap (loopPool pool) dirs

    where loopPool :: Pool -> FilePath -> IO ()
          loopPool pool fp = do
                all' <- fmap (fp </>) <$> listDirectory fp
                -- unsafeInterleaveIO? lol
                (dirs', files') <- partitionM doesDirectoryExist all'
                dirs'' <- filterM dirP dirs'
                files'' <- filterM fileP files'
                parallel_ pool (fmap act files'' ++ fmap (loopPool pool) dirs'')

parTraverse :: (FilePath -> IO ()) -- ^ Action to execute on files
            -> (FilePath -> IO Bool) -- ^ Filter on files
            -> (FilePath -> IO Bool) -- ^ Filter on directories
            -> [FilePath] -- ^ Starting directories
            -> IO ()
parTraverse act fileP dirP dirs = parTraverseFiles act fileP dirP dirs []
