#!/usr/bin/env cabal
{- cabal:
build-depends: base, shake, shake-ext
default-language: Haskell2010
ghc-options: -Wall -threaded -rtsopts "-with-rtsopts=-I0 -qg -qb"
-}

import           Development.Shake
import           Development.Shake.FileDetect
import           Development.Shake.Linters

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "dist-newstyle", shakeLint = Just LintBasic, shakeChange = ChangeModtimeAndDigestInput } $ do
    want [ "lint" ]

    "lint" ~> do
        ymlSrc <- getYml
        unit $ command [] "yamllint" ymlSrc
        unit $ command [] "hlint" ["src", "app", "test", "bench", "par-traverse/src", "shake.hs"]
        unit $ command [] "shellcheck" ["bash/bench"]
        hsSrc <- getHs ["par-traverse/src", "src", "test", "app", "bench"]
        stylishHaskell ("shake.hs" : hsSrc)
        command [] "shellcheck" ["-e", "SC2016", "bash/install.sh"]
