# phash

  * Better performance on `.webp` images

## 0.1.4.2

  * Detect `.webp` on the command-line

## 0.1.4.1

  * Add support for [webp](https://developers.google.com/speed/webp) images.

## 0.1.4.0

  * Add `hammingDistance` function in `PerceptualHash` module

## 0.1.3.5

  * Allows files to be passed via the command-line

## 0.1.3.3

  * Fix bug

## 0.1.3.2

  * Fix `include/hs_phash.h` installation

## 0.1.3.1

  * Add `include/hs_phash.h` header

## 0.1.3.0

  * CLI tool now supports several more image formats
  * `--debug` flag more useful
  * `fileHash` now returns an `Either String`

## 0.1.2.0

  * Add `demo-data` field so tests don't fail
  * Generalize `imgHash` signature

## 0.1.1.1

  * Use mirror for convolutions
  * Add `llvm` cabal flag

## 0.1.1.0

  * Add `with-phash` flag to enable `ForeignHash` module and benchmarks
    against pHash
  * Significant performance improvements
  * Generalized signature for `imgHash`

## 0.1.0.2

  * Performance improvements
  * Mean filter implemented correctly

## 0.1.0.1

  * Update to work with latest `par-traverse`

## 0.1.0.0

Initial release
