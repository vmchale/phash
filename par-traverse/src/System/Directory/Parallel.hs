module System.Directory.Parallel ( parTraverse ) where

import           Control.Concurrent                  (getNumCapabilities)
import           Control.Concurrent.ParallelIO.Local (Pool, parallel_, withPool)
import           System.Directory                    (doesDirectoryExist,
                                                      listDirectory)
import           System.FilePath                     ((</>))

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    if res
        then pure (x:as, bs)
        else pure (as, x:bs)

parTraverse :: (FilePath -> IO ()) -- ^ Action to execute on files
            -> FilePath -- ^ Starting directory
            -> IO ()
parTraverse act dir = do
    ncpu <- getNumCapabilities
    withPool ncpu $ \pool ->
        loopPool pool dir

    where loopPool :: Pool -> FilePath -> IO ()
          loopPool pool fp = do
                all' <- fmap (fp </>) <$> listDirectory fp
                (dirs, files) <- partitionM doesDirectoryExist all'
                parallel_ pool (fmap act files ++ fmap (loopPool pool) dirs)
