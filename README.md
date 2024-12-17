# phash

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

For some platforms, you can find binaries [here](https://www.permanent.org/p/archive/0236-0000/0236-0019/410841).

### Source

Download [cabal-install](https://www.haskell.org/cabal/download.html) and
[GHC](https://www.haskell.org/ghc/download.html). Then:

```
cabal install perceptual-hash --constraint='hip +disable-chart'
```

You may need

```cabal
package hip
  ghc-options: -fsimpl-tick-factor=200
```

### Performance

This library has performs better on WebP, AVIF images and worse on JPEG images compared to the pHash library.

```
benchmarking fileHash/cat.png
time                 21.50 ms   (21.23 ms .. 21.70 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 21.54 ms   (21.39 ms .. 21.88 ms)
std dev              525.3 μs   (224.2 μs .. 946.8 μs)

benchmarking fileHash/frog.jpeg
time                 21.33 ms   (20.57 ms .. 22.01 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 19.84 ms   (19.53 ms .. 20.24 ms)
std dev              822.7 μs   (654.3 μs .. 1.070 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking fileHash/frog.png
time                 12.31 ms   (11.88 ms .. 12.66 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 12.46 ms   (12.34 ms .. 12.58 ms)
std dev              314.3 μs   (256.0 μs .. 385.0 μs)

benchmarking fileHash/liz-taylor.webp
time                 69.02 ms   (68.00 ms .. 70.23 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 71.69 ms   (70.28 ms .. 74.98 ms)
std dev              3.688 ms   (1.345 ms .. 6.107 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking fileHash/liz-taylor.png
time                 72.32 ms   (70.35 ms .. 74.49 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 73.83 ms   (72.60 ms .. 75.38 ms)
std dev              2.414 ms   (1.655 ms .. 3.614 ms)

benchmarking fileHash/fashion.png
time                 142.0 ms   (131.6 ms .. 153.4 ms)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 145.8 ms   (142.0 ms .. 153.9 ms)
std dev              7.690 ms   (2.694 ms .. 11.43 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking fileHash/fashion.avif
time                 172.2 ms   (161.5 ms .. 187.6 ms)
                     0.996 R²   (0.991 R² .. 1.000 R²)
mean                 175.1 ms   (169.3 ms .. 185.3 ms)
std dev              12.16 ms   (5.052 ms .. 18.42 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking foreignHash/cat.png
time                 21.06 ms   (20.88 ms .. 21.19 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 20.94 ms   (20.80 ms .. 21.06 ms)
std dev              320.2 μs   (225.5 μs .. 418.3 μs)

benchmarking foreignHash/frog.jpeg
time                 9.255 ms   (9.168 ms .. 9.378 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.234 ms   (9.201 ms .. 9.273 ms)
std dev              94.68 μs   (79.14 μs .. 120.7 μs)

benchmarking foreignHash/frog.png
time                 10.37 ms   (10.28 ms .. 10.47 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 10.42 ms   (10.37 ms .. 10.48 ms)
std dev              135.4 μs   (106.5 μs .. 184.9 μs)

benchmarking foreignHash/liz-taylor.webp
time                 368.7 ms   (352.4 ms .. 391.1 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 362.3 ms   (357.6 ms .. 366.4 ms)
std dev              5.133 ms   (1.154 ms .. 6.530 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking foreignHash/liz-taylor.png
time                 71.58 ms   (71.12 ms .. 72.03 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 71.84 ms   (71.66 ms .. 72.24 ms)
std dev              502.0 μs   (339.8 μs .. 710.2 μs)

benchmarking foreignHash/fashion.png
time                 162.1 ms   (160.6 ms .. 162.8 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 163.4 ms   (162.9 ms .. 164.3 ms)
std dev              990.0 μs   (544.6 μs .. 1.476 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking foreignHash/fashion.avif
time                 705.5 ms   (669.6 ms .. 757.2 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 716.4 ms   (710.3 ms .. 728.3 ms)
std dev              11.54 ms   (1.129 ms .. 13.84 ms)
variance introduced by outliers: 19% (moderately inflated)
```

### Foreign Library

This package contains a foreign library and a [header
file](https://hackage.haskell.org/package/perceptual-hash/src/include/hs_phash.h)
