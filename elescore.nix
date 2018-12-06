{ mkDerivation, aeson, async, attoparsec, base, bytestring
, classy-prelude, containers, datetime, exceptions, free, hpack
, http-api-data, http-client, http-client-tls, lens, mtl, pipes
, pipes-concurrency, raw-strings-qq, scalpel-core, servant
, servant-client, servant-pagination, servant-server, signal
, sqlite-simple, stdenv, tagsoup, text, time, unordered-containers
, uri-bytestring, uuid, warp, wreq
}:
mkDerivation {
  pname = "elescore";
  version = "3.2.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base bytestring classy-prelude containers
    datetime exceptions free http-api-data http-client http-client-tls
    lens mtl pipes pipes-concurrency raw-strings-qq scalpel-core
    servant servant-client servant-pagination servant-server signal
    sqlite-simple tagsoup text time unordered-containers uri-bytestring
    uuid warp wreq
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/akii/elescore#readme";
  license = stdenv.lib.licenses.bsd3;
}
