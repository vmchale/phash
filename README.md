# phash

[![Build Status](https://travis-ci.org/vmchale/phash.svg?branch=master)](https://travis-ci.org/vmchale/phash)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/vmchale/phash?svg=true)](https://ci.appveyor.com/project/vmchale/phash)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/perceptual-hash/badge)](https://matrix.hackage.haskell.org/package/perceptual-hash)
[![Hackage](https://img.shields.io/hackage/v/perceptual-hash.svg)](http://hackage.haskell.org/package/perceptual-hash)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/perceptual-hash.svg)](https://hackage.haskell.org/package/perceptual-hash)

This is a Haskell [library](http://hackage.haskell.org/package/perceptual-hash)
to detect (potential) duplicate images.

It also contains a command-line tool.

- [Use](#use)
- [Installation](#installation)
  - [Script](#script)
  - [Pre-Built Release](#pre-built-release)
  - [Source](#source)
- [Library](#library)
  - [Performance](#performance)
  - [Foreign Library](#foreign-library)

## Use

Use it on one or more directories:

```
phash ~/Pictures ~/Downloads
~/Pictures/frog.jpeg, ~/Downloads/frog.png
```

## Installation

### Script

On many platforms, you can install the command-line tool with a script:

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

This library has similar performance to the pHash library for PNG
images.

```
benchmarking fileHash/cat.png
time                 20.67 ms   (20.60 ms .. 20.78 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 20.84 ms   (20.76 ms .. 21.01 ms)
std dev              246.3 μs   (149.5 μs .. 396.7 μs)

benchmarking fileHash/frog.jpeg
time                 17.82 ms   (17.62 ms .. 18.05 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 17.54 ms   (17.45 ms .. 17.65 ms)
std dev              272.3 μs   (194.9 μs .. 369.3 μs)

benchmarking fileHash/frog.png
time                 12.02 ms   (11.95 ms .. 12.11 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 12.08 ms   (12.04 ms .. 12.13 ms)
std dev              115.0 μs   (90.79 μs .. 144.7 μs)

benchmarking foreignHash/cat.png
time                 18.86 ms   (18.79 ms .. 18.97 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 18.92 ms   (18.87 ms .. 18.99 ms)
std dev              149.7 μs   (129.0 μs .. 183.6 μs)

benchmarking foreignHash/frog.jpeg
time                 8.533 ms   (8.480 ms .. 8.574 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 8.686 ms   (8.644 ms .. 8.745 ms)
std dev              129.0 μs   (97.99 μs .. 164.8 μs)

benchmarking foreignHash/frog.png
time                 9.697 ms   (9.649 ms .. 9.735 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.746 ms   (9.717 ms .. 9.775 ms)
std dev              84.08 μs   (70.14 μs .. 97.91 μs)
```

### Foreign Library

This package contains a foreign library and a [header
file](https://hackage.haskell.org/package/perceptual-hash/src/include/hs_phash.h)
