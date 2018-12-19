{ nixpkgs ? import <nixpkgs> {} }:

let

  jsaddle = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "34fe7d61b3f387b81aa748294ac8d993243f53b4";
    sha256 = "0qdh5qdk23vcp1yp910zgw2hs4zpbx9ig25xgaax0iwj2m1ifh5x";
  };
  config = {
    packageOverrides = pkgs: with pkgs.haskell.lib;  with pkgs.lib; {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghccustom = pkgs.haskell.packages.ghc843.override {
            overrides = self: super: {
              jsaddle-warp = dontCheck (super.callPackage (jsaddle + "/jsaddle-warp") {});
              # jsaddle-warp = super.callPackage ./jsaddle-warp-ghcjs.nix {};
              jsaddle = dontCheck (super.callPackage (jsaddle + "/jsaddle") {});
            };
          };
          ghcjscustom = pkgs.haskell.packages.ghcjs84.override {
            overrides = self: super: {
              doctest = null;
              comonad = dontCheck (super.comonad);
              classy-prelude = dontCheck (super.classy-prelude);
              unliftio = dontCheck (super.unliftio);
              semigroupoids = dontCheck (super.semigroupoids);
              lens = dontCheck (super.lens);
              directory-tree = dontCheck (super.directory-tree);
              http-types = dontCheck (super.http-types);
              tasty-quickcheck = dontCheck (super.tasty-quickcheck);
              scientific = dontCheck (super.scientific);
              servant = dontCheck (super.servant);
              jsaddle-warp = super.callPackage ./jsaddle-warp-ghcjs.nix {};
              ghc = overrideDerivation (super.ghc.override {
                ghcjsSrc = pkgs.fetchgit {
                  url = "https://github.com/ghcjs/ghcjs.git";
                  rev = "dc190b1bb2453cfa484124e9f335ee3cad1492f7";
                  sha256 = "0dh52gj0f3700zfyrhisy44b6y9p1bsawwrmd5pllpdyw21zd9lw";
                  fetchSubmodules = true;
                };
              }) (drv: { patches = (drv.patches or []) ++ [ ./ghcjs.patch ]; });
            };
          };
        };
      };
    };
  };
  pinnedVersion = nixpkgs.lib.importJSON ./nixpkgs-version.json;
  pinnedPkgs = import (nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (pinnedVersion) rev sha256;
  }) { inherit config; };
  ghc = import ./default.nix { nixpkgs = pinnedPkgs; haskellPackages = pinnedPkgs.haskell.packages.ghccustom; };
  ghcjs = import ./default.nix { nixpkgs = pinnedPkgs; haskellPackages = pinnedPkgs.haskell.packages.ghcjscustom; };
  inherit (pinnedPkgs) pkgs;

  in

{
  inherit ghc ghcjs;
  deps = pkgs.buildEnv {
    name = "deps";
    paths = [
      (pkgs.haskell.packages.ghccustom.ghcWithPackages (_: ghc.backend.buildInputs ++ ghc.backend.propagatedBuildInputs))
      (pkgs.haskell.packages.ghcjscustom.ghcWithPackages (_: ghcjs.frontend.buildInputs ++ ghcjs.frontend.propagatedBuildInputs))
    ];
    buildInputs = [ ];
  };
}
