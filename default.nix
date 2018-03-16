{ mkDerivation, base, constraint-classes, constraints, deepseq
, newtype, stdenv
}:
mkDerivation {
  pname = "phantom-index";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base constraint-classes constraints deepseq newtype
  ];
  description = "Phantom indexes for data constructors";
  license = stdenv.lib.licenses.bsd3;
}
