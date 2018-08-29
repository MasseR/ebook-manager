{ nixpkgs, haskellPackages }:

(import ./project.nix nixpkgs) {
  packages = {
    common = ./common;
    backend = ./backend;
  };
  tools = with haskellPackages; [
    ghcid
    hasktags
  ];
}
