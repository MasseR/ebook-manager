{ nixpkgs ? import <nixpkgs> {} }:

let

  pinnedVersion = nixpkgs.lib.importJSON ./nixpkgs-version.json;
  pinnedPkgs = import (nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (pinnedVersion) rev sha256;
  }) {};
  inherit (pinnedPkgs) pkgs;

  in

import ./default.nix { nixpkgs = pinnedPkgs; haskellPackages = pinnedPkgs.haskellPackages; }
