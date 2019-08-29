# phash

[![Build Status](https://travis-ci.org/vmchale/phash.svg?branch=master)](https://travis-ci.org/vmchale/phash)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/vmchale/phash?svg=true)](https://ci.appveyor.com/project/vmchale/phash)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/perceptual-hash/badge)](https://matrix.hackage.haskell.org/package/perceptual-hash)
[![Hackage](https://img.shields.io/hackage/v/perceptual-hash.svg)](http://hackage.haskell.org/package/perceptual-hash)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/perceptual-hash.svg)](https://hackage.haskell.org/package/perceptual-hash)

This is a command-line tool to detect (potential) duplicate images. It also
contains a Haskell [library](http://hackage.haskell.org/package/perceptual-hash).

## Use

Use it on one or more directories:

```
phash ~/Pictures ~/Downloads
~/Pictures/frog.jpeg, ~/Downloads/frog.png
```

## Installation

### Script

On many platforms, you can install with a script, viz.

```
curl -sSl https://raw.githubusercontent.com/vmchale/phash/master/bash/install.sh | sh -s
```

### Pre-Built Release

Download the latest release from
[here](https://github.com/vmchale/phash/releases).

### Source

Download [cabal-install](https://www.haskell.org/cabal/download.html) and
[GHC](https://www.haskell.org/ghc/download.html). Then:

```
cabal install perceptual-hash -w ghc-8.6.5
```
