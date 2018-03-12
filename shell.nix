{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, constraint-classes, newtype, stdenv }:
      mkDerivation {
        pname = "phantom-index";
        version = "0.3.0";
        src = ./.;
        libraryHaskellDepends = [ base constraint-classes newtype ];
        description = "Phantom indexes for data constructors";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
