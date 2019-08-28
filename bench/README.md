To run all the benchmarks, first download and install
[pHash](http://phash.org/download/).

I installed pHash with [cpkg](http://hackage.haskell.org/package/cpkg), viz.

```
cpkg install pHash -vv
LD_LIBRARY_PATH=$(cpkg dump ld-path pHash) PKG_CONFIG_PATH=$(cpkg dump pkg-config pHash) cabal new-bench --constraint='perceptual-hash +with-phash' --constraint='perceptual-hash +llvm'
```
