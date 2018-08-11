let
  config = {
    allowUnfree = true;
  };
  hostPkgs = import <nixpkgs> {};
  pinnedVersion = hostPkgs.lib.importJSON ./nix/nixpkgs-version.json;
  pinnedPkgs = hostPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    inherit (pinnedVersion) rev sha256;
  };
  ghc = hostPkgs.haskell.packages.ghc822.ghcWithPackages
        (hpkgs: with hpkgs; [
        clay
        ]);

in with import pinnedPkgs { inherit config; };
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    purescript
    ghc
  ];
}
