{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  pinnedVersion = nixpkgs.lib.importJSON ./nixpkgs-version.json;
  pinnedPkgs = import (nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (pinnedVersion) rev sha256;
  }) {};
  emailparse = nixpkgs.fetchFromGitHub {
    owner = "mkawalec";
    repo = "emailparse";
    rev = "c5533707c7339bcebd616e1f7cdb442ca2d16626";
    sha256 = "036a6yx6lqjz2prj85vi1qxcy9ph4mnr0dq1djj3j7y1rqhmgyiq";
  };
  inherit (pinnedPkgs) pkgs;


  haskellPackages = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  f = haskellPackages.callCabal2nix "ebook-manager" ./.;

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
