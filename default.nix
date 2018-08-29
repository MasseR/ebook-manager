{ nixpkgs, haskellPackages }:

(import ./project.nix nixpkgs) {
  packages = {
    common = ./common;
    backend = ./backend;
  };
}
