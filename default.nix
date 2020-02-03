{ mkDerivation, base, binary, bytestring, containers, mtl, random
, stdenv, text
}:
mkDerivation {
  pname = "genetic-algorithm";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers mtl random text
  ];
  license = stdenv.lib.licenses.bsd3;
}
