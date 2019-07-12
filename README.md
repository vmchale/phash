# phash

This is a command-line tool to detect duplicate images.

## Installation

### Install pHash

You can install [pHash](http://phash.org/) using
[cpkg](http://hackage.haskell.org/package/cpkg). This includes some patches
necessary for pHash.

```
PKG_CONFIG_PATH="$(cpkg dump pkg-config pHash)" cabal new-install exe:phash
```
