{ mkDerivation, async, attoparsec, base, base64-bytestring, binary
, bytestring, containers, criterion, directory, ekg, exceptions
, filemanip, filepath, hashable, hedis, hinotify, hspec
, hspec-attoparsec, lens, monad-control, monad-logger, mtl
, pcre-light, protolude-lifted, QuickCheck, quickcheck-instances
, regex-applicative, stdenv, stm, stringsearch, text, time
, unordered-containers
}:
mkDerivation {
  pname = "ngx-cache-purge";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base base64-bytestring binary bytestring
    containers directory filemanip filepath hashable hedis hinotify
    lens monad-control monad-logger mtl pcre-light protolude-lifted
    regex-applicative stm stringsearch text time unordered-containers
  ];
  executableHaskellDepends = [
    attoparsec base ekg monad-logger protolude-lifted
  ];
  testHaskellDepends = [
    attoparsec base binary bytestring containers directory exceptions
    hspec hspec-attoparsec lens mtl pcre-light protolude-lifted
    QuickCheck quickcheck-instances stringsearch unordered-containers
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring criterion protolude-lifted
  ];
  description = "Purges nginx caches";
  license = stdenv.lib.licenses.bsd3;
}
