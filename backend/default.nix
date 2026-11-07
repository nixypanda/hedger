{
  mkDerivation,
  aeson,
  async,
  base,
  bytestring,
  containers,
  cryptohash-md5,
  Decimal,
  deepseq,
  directory,
  filepath,
  fsnotify,
  hedgehog,
  hledger-lib,
  http-types,
  lib,
  math-functions,
  mtl,
  network,
  optparse-applicative,
  scientific,
  servant,
  servant-server,
  stm,
  tagged,
  tasty,
  tasty-bench,
  tasty-hedgehog,
  tasty-hunit,
  text,
  time,
  transformers,
  unix,
  uuid,
  vector,
  wai,
  wai-extra,
  warp,
  yaml,
}:
mkDerivation {
  pname = "hedger-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    async
    base
    bytestring
    containers
    cryptohash-md5
    Decimal
    directory
    filepath
    fsnotify
    hledger-lib
    http-types
    math-functions
    mtl
    optparse-applicative
    scientific
    servant
    servant-server
    stm
    tagged
    text
    time
    transformers
    unix
    uuid
    wai
    wai-extra
    warp
    yaml
  ];
  executableHaskellDepends = [
    base
    text
  ];
  testHaskellDepends = [
    aeson
    base
    bytestring
    containers
    Decimal
    directory
    filepath
    hedgehog
    hledger-lib
    http-types
    math-functions
    network
    tasty
    tasty-hedgehog
    tasty-hunit
    text
    time
    vector
    wai
    wai-extra
    warp
  ];
  benchmarkHaskellDepends = [
    aeson
    base
    bytestring
    containers
    Decimal
    deepseq
    hledger-lib
    tasty
    tasty-bench
    text
    time
  ];
  description = "Read-only hledger backend for Hedger";
  license = lib.meta.getLicenseFromSpdxId "AGPL-3.0-or-later";
  mainProgram = "hedger-server";
}
