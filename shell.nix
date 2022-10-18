{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  pkgs = import <nixpkgs> { config = {}};
in
  pkgs.mkShell { 
     shellHook = ''
       export NIX_PATH="nixpkgs=nixpkgs-17.09"
     '';
  }
  
let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, gloss, gloss-rendering, elerea, stdenv }:
      mkDerivation {
        pname = "mutorere";
        version = "1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base gloss gloss-rendering ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
