#!/usr/bin/env cabal
{- cabal:
build-depends: base, shake, shake-ext
default-language: Haskell2010
ghc-options: -Wall -threaded -rtsopts "-with-rtsopts=-I0 -qg -qb"
-}

import           Development.Shake
import           Development.Shake.FileDetect

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".shake", shakeLint = Just LintBasic, shakeChange = ChangeModtimeAndDigestInput } $ do
    want [ "lint" ]

    "lint" ~> do
        ymlSrc <- getYml
        unit $ command [] "yamllint" ymlSrc
        unit $ command [] "hlint" ["src", "app", "test", "bench", "par-traverse/src", "shake.hs"]
        unit $ command [] "shellcheck" ["bash/bench"]
        command [] "shellcheck" ["-e", "SC2016", "bash/install.sh"]

