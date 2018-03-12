{ mkDerivation, base, constraint-classes, deepseq, newtype, stdenv
}:
mkDerivation {
  pname = "phantom-index";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base constraint-classes deepseq newtype
  ];
  description = "Phantom indexes for data constructors";
  license = stdenv.lib.licenses.bsd3;
}
