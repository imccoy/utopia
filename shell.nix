{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bimap, bytestring, containers
      , edit-distance, lens, mtl, safe, stdenv, text, uuid, wreq
      }:
      mkDerivation {
        pname = "utopia";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base edit-distance safe text ];
        executableHaskellDepends = [
          base bimap bytestring containers lens mtl safe text uuid wreq
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
