{ mkDerivation, base, hspec, stdenv }:
mkDerivation {
  pname = "phash";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/vmchale/phash#readme";
  license = stdenv.lib.licenses.bsd3;
}
