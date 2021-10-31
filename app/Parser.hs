module Parser ( wrapper ) where

import           Data.Semigroup        ((<>))
import           Data.Version          as V
import           Options.Applicative
import qualified Paths_perceptual_hash as P

phashVersion :: V.Version
phashVersion = P.version

debug :: Parser Bool
debug =
    switch
    (long "debug"
    <> help "Show debug output")

threaded :: Parser Bool
threaded =
    switch
    (long "no-threads"
    <> help "Disable multicore")

targets :: Parser ([FilePath], Bool, Bool)
targets = (,,)
    <$> some (argument str
             (metavar "DIRECTORY"
             <> help "Directory to include"))
    <*> debug
    <*> threaded

wrapper :: ParserInfo ([FilePath], Bool, Bool)
wrapper = info (helper <* versionInfo <*> targets)
    (fullDesc
    <> progDesc "A command-line tool to detect duplicate images."
    <> header "phash - find duplicates using perceptual hashes")

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("phash version: " ++ V.showVersion phashVersion) (short 'V' <> long "version" <> help "Show version")
