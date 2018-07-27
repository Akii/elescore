{ mkDerivation, aeson, async, attoparsec, base, bytestring
, classy-prelude, containers, datetime, exceptions, free
, http-api-data, http-client, http-client-tls, mtl, pipes
, pipes-concurrency, raw-strings-qq, servant, servant-client
, servant-server, signal, sqlite-simple, stdenv, text, tinylog
, uuid, warp
}:
mkDerivation {
  pname = "elescore";
  version = "2.3.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring classy-prelude containers
    datetime exceptions free http-api-data http-client http-client-tls
    mtl pipes pipes-concurrency raw-strings-qq servant servant-client
    servant-server signal sqlite-simple text tinylog uuid warp
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/akii/elescore#readme";
  license = stdenv.lib.licenses.bsd3;
}
