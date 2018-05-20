{ mkDerivation, aeson, async, attoparsec, base, base16-bytestring
, bytestring, classy-prelude, containers, cryptonite, datetime
, email-validate, exceptions, free, http-api-data, http-client
, http-client-tls, mime-mail, mtl, optparse-applicative, pipes
, pipes-concurrency, pwstore-fast, servant, servant-client
, servant-server, stdenv, text, uuid, warp
}:
mkDerivation {
  pname = "elescore";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base base16-bytestring bytestring
    classy-prelude containers cryptonite datetime email-validate
    exceptions free http-api-data http-client http-client-tls mime-mail
    mtl optparse-applicative pipes pipes-concurrency pwstore-fast
    servant servant-client servant-server text uuid warp
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/akii/elescore#readme";
  license = stdenv.lib.licenses.bsd3;
}
