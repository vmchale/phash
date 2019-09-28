# phash

[![Build Status](https://travis-ci.org/vmchale/phash.svg?branch=master)](https://travis-ci.org/vmchale/phash)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/vmchale/phash?svg=true)](https://ci.appveyor.com/project/vmchale/phash)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/perceptual-hash/badge)](https://matrix.hackage.haskell.org/package/perceptual-hash)
[![Hackage](https://img.shields.io/hackage/v/perceptual-hash.svg)](http://hackage.haskell.org/package/perceptual-hash)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/perceptual-hash.svg)](https://hackage.haskell.org/package/perceptual-hash)

This is a Haskell [library](http://hackage.haskell.org/package/perceptual-hash)
to detect (potential) duplicate images.

It also contains a command-line tool.

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

## Library

You can find library documentation on
[Hackage](https://hackage.haskell.org/package/perceptual-hash).

### Performance

This library is more performant than the pHash library for PNG
images.

```
benchmarking fileHash/cat.png
time                 21.42 ms   (21.20 ms .. 21.63 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 21.34 ms   (21.28 ms .. 21.44 ms)
std dev              184.3 μs   (93.40 μs .. 266.3 μs)

benchmarking fileHash/frog.jpeg
time                 18.16 ms   (18.07 ms .. 18.24 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 18.24 ms   (18.12 ms .. 18.31 ms)
std dev              230.1 μs   (161.3 μs .. 360.2 μs)

benchmarking fileHash/frog.png
time                 12.39 ms   (12.35 ms .. 12.46 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 12.38 ms   (12.34 ms .. 12.43 ms)
std dev              114.0 μs   (73.36 μs .. 156.5 μs)

benchmarking foreignHash/cat.png
time                 27.21 ms   (27.09 ms .. 27.47 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 27.24 ms   (27.18 ms .. 27.37 ms)
std dev              207.9 μs   (84.97 μs .. 311.8 μs)

benchmarking foreignHash/frog.jpeg
time                 12.81 ms   (12.78 ms .. 12.84 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.87 ms   (12.85 ms .. 12.92 ms)
std dev              80.81 μs   (56.17 μs .. 113.1 μs)

benchmarking foreignHash/frog.png
time                 14.03 ms   (13.94 ms .. 14.13 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 13.95 ms   (13.93 ms .. 13.99 ms)
std dev              79.36 μs   (52.02 μs .. 119.5 μs)
```

### Foreign Library

This package contains a foreign library and header files.
