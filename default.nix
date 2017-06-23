{ mkDerivation, aeson, aeson-compat, base, base-compat
, dependent-map, dependent-sum-template, lens, lens-aeson, mtl
, servant, servant-server, stdenv, text, transformers, wreq
}:
mkDerivation {
  pname = "weather";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-compat base base-compat dependent-map
    dependent-sum-template lens lens-aeson mtl servant servant-server
    text transformers wreq
  ];
  license = stdenv.lib.licenses.bsd3;
}
