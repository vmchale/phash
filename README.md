# phash

[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/perceptual-hash/badge)](https://matrix.hackage.haskell.org/package/perceptual-hash)
[![Hackage](https://img.shields.io/hackage/v/perceptual-hash.svg)](http://hackage.haskell.org/package/perceptual-hash)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/perceptual-hash.svg)](https://hackage.haskell.org/package/perceptual-hash)

This is a Haskell [library](http://hackage.haskell.org/package/perceptual-hash)
to detect (potential) duplicate images.

It also contains a command-line tool.

- [Use](#use)
- [Installation](#installation)
  - [Pre-Built Release](#pre-built-release)
  - [Source](#source)
- [Library](#library)
  - [Performance](#performance)
  - [Foreign Library](#foreign-library)
    - [ATS Bindings](#ats-bindings)

## Use

Use it on one or more directories:

```
phash ~/Pictures ~/Downloads
~/Pictures/frog.jpeg, ~/Downloads/frog.png
```

## Installation

### Pre-Built Release

For many platforms, you can find binaries [here](https://www.permanent.org/p/archive/0236-0000/0236-0019/410841).

### Source

Download [cabal-install](https://www.haskell.org/cabal/download.html) and
[GHC](https://www.haskell.org/ghc/download.html). Then:

```
cabal install perceptual-hash --constraint='hip +disable-chart'
```

## Library

You can find library documentation on
[Hackage](https://hackage.haskell.org/package/perceptual-hash).

### Performance

This library has varying performance compared to the pHash library.

```
benchmarking fileHash/cat.png
time                 20.84 ms   (20.63 ms .. 21.00 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 21.00 ms   (20.89 ms .. 21.30 ms)
std dev              393.4 μs   (172.3 μs .. 722.2 μs)

benchmarking fileHash/frog.jpeg
time                 18.66 ms   (18.50 ms .. 18.84 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 18.44 ms   (18.34 ms .. 18.54 ms)
std dev              230.2 μs   (167.5 μs .. 303.0 μs)

benchmarking fileHash/frog.png
time                 11.78 ms   (11.70 ms .. 11.86 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 11.81 ms   (11.78 ms .. 11.86 ms)
std dev              103.9 μs   (74.32 μs .. 149.0 μs)

benchmarking fileHash/liz-taylor.webp
time                 106.8 ms   (105.2 ms .. 108.1 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 109.5 ms   (108.2 ms .. 111.7 ms)
std dev              2.642 ms   (1.546 ms .. 4.081 ms)

benchmarking fileHash/liz-taylor.png
time                 71.42 ms   (70.83 ms .. 72.24 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 72.60 ms   (71.71 ms .. 74.85 ms)
std dev              2.304 ms   (688.1 μs .. 3.802 ms)

benchmarking foreignHash/cat.png
time                 23.12 ms   (23.00 ms .. 23.21 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 23.15 ms   (23.09 ms .. 23.22 ms)
std dev              151.9 μs   (121.8 μs .. 210.6 μs)

benchmarking foreignHash/frog.jpeg
time                 8.430 ms   (8.266 ms .. 8.582 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 8.313 ms   (8.286 ms .. 8.377 ms)
std dev              111.3 μs   (70.34 μs .. 194.5 μs)

benchmarking foreignHash/frog.png
time                 11.78 ms   (11.74 ms .. 11.83 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.80 ms   (11.77 ms .. 11.82 ms)
std dev              80.07 μs   (63.46 μs .. 98.00 μs)

benchmarking foreignHash/liz-taylor.webp
time                 331.4 ms   (329.3 ms .. 333.7 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 331.6 ms   (330.7 ms .. 332.6 ms)
std dev              1.212 ms   (920.0 μs .. 1.524 ms)
variance introduced by outliers: 16% (moderately inflated)
```

### Foreign Library

This package contains a foreign library and a [header
file](https://hackage.haskell.org/package/perceptual-hash/src/include/hs_phash.h)

#### ATS Bindings

Because of the foreign library, `hs_phash` can be used in ATS as well. ATS users
of the library may be interested in [hs-bind](https://github.com/vmchale/hs-bind).
