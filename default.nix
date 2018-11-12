{ nixpkgs, haskellPackages }:

let
  miso = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "630e823dd40a434b73124e12b229a79d9fefb01d";
    sha256 = "046gdp3ah2lsipfcy89rh20mn08xbhcgrj549v8zzy69j33xjm2l";
  };
  miso-jsaddle = super: if haskellPackages.ghc.isGhcjs or false then (super.callPackage (miso + "/miso-ghcjs.nix") {}) else (super.callPackage (miso + "/miso-ghc-jsaddle.nix") {});

  dontCheck = nixpkgs.haskell.lib.dontCheck;

in

(import ./project.nix nixpkgs) {
  haskellPackages = haskellPackages;
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };
  overrides = self: super: {
    generic-lens = nixpkgs.haskell.lib.dontCheck super.generic-lens;
    miso = miso-jsaddle super;
    # doctest = null; # Not compilable with ghcjs
    # comonad = dontCheck super.comonad;
    # Glob = dontCheck super.Glob;
    # SHA = dontCheck super.SHA;
    # iproute = dontCheck super.iproute;
    # semigroupoids = dontCheck super.semigroupoids;
    # wai-app-static = dontCheck super.wai-app-static;
    # attoparsec = dontCheck super.attoparsec;
    # http-date = dontCheck super.http-date;
    # lens = dontCheck super.lens;
    # unix-time = dontCheck super.unix-time;
    # http-types = dontCheck super.http-types;
    # servant = dontCheck super.servant;
    # servant-server = dontCheck super.servant-server;
    # servant-auth-docs = dontCheck super.servant-auth-docs;
    # lens-aeson = dontCheck super.lens-aeson;
    # word8 = dontCheck super.word8;
    # http2 = dontCheck super.http2;
    # wai-extra = dontCheck super.wai-extra;
    # pgp-wordlist = dontCheck super.pgp-wordlist;
    # prettyprinter = dontCheck super.prettyprinter;
    # unliftio = dontCheck super.unliftio;
    # prettyprinter-ansi-terminal = dontCheck super.prettyprinter-ansi-terminal;
    # distributive = dontCheck super.distributive;
    # genvalidity-property = dontCheck super.genvalidity-property;
    # genvalidity-hspec = dontCheck super.genvalidity-hspec;
    # genvalidity = dontCheck super.genvalidity;
    # megaparsec = dontCheck super.megaparsec;
    # ncurses = null;
    # haskeline = super.callHackage "haskeline" "0.7.4.2" {};
    # terminfo = super.callHackage "terminfo" "0.4.1.1" {};
  };
  tools = with haskellPackages; [
    ghcid
    hasktags
  ];
}
