nixpkgs:

let

  inherit (nixpkgs.lib) mapAttrs mapAttrsToList escapeShellArg optionalString concatStringsSep concatMapStringsSep;

in

{ packages
, overrides ? _ : _ : {}
, tools ? []
}:

let

  overrides' = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
    (self: super: mapAttrs (name: path: self.callCabal2nix name path {}) packages)
  ];
  haskellPackages = nixpkgs.haskellPackages.override { overrides = overrides'; };
  packages' = mapAttrs (name: _: haskellPackages."${name}") packages;
  mkShell = name: pkg:
  let
    n =  "${name}-shell";
    deps = haskellPackages.ghcWithPackages (pkgs: pkg.nativeBuildInputs);
  in
  {
    name = "${n}";
    value = nixpkgs.buildEnv {
      name = "${n}";
      paths = tools;
      buildInputs = tools ++ [deps];
    };
  };
  shells = nixpkgs.lib.listToAttrs (mapAttrsToList mkShell packages');

in

packages' // shells
