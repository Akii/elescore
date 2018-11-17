{ mkDerivation, aeson, async, attoparsec, base, bytestring
, classy-prelude, containers, datetime, exceptions, free
, http-api-data, http-client, http-client-tls, lens, mtl, pipes
, pipes-concurrency, raw-strings-qq, servant, servant-client
, servant-server, signal, sqlite-simple, stdenv, text, time
, tinylog, unordered-containers, uuid, warp, scalpel-core, tagsoup
, wreq
}:
mkDerivation {
  pname = "elescore";
  version = "3.1.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring classy-prelude containers
    datetime exceptions free http-api-data http-client http-client-tls
    lens mtl pipes pipes-concurrency raw-strings-qq servant
    servant-client servant-server signal sqlite-simple text time
    tinylog unordered-containers uuid warp scalpel-core tagsoup wreq
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/akii/elescore#readme";
  license = stdenv.lib.licenses.bsd3;
}
