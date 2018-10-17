{ nixpkgs, haskellPackages }:

(import ./project.nix nixpkgs) {
  packages = {
    common = ./common;
    backend = ./backend;
  };
  overrides = self: super: {
    generic-lens = nixpkgs.haskell.lib.dontCheck super.generic-lens;
  };
  tools = with haskellPackages; [
    ghcid
    hasktags
  ];
}
