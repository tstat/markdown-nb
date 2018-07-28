{ compiler ? "ghc843" }:

let mitchell-overlay =
  let haskell-overlay = self: super:
    with super.haskell.lib; let inherit (super) callPackage; in {
      yi-rope = callPackage ./nix/yi-rope.nix {};
      base-orphans = callPackage ./nix/base-orphans.nix {};
      aeson = super.callPackage ./nix/aeson.nix {};
      mitchell-stdlib = self.callPackage ./nix/mitchell-stdlib.nix { };
      markdown-nb-server = self.callPackage ./nix/default.nix {};
    };
  in self: super:
      {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            "${compiler}" = super.haskell.packages."${compiler}".override {
              overrides = haskell-overlay;
            };
          };
        };
      };
    pkgs = import <nixos-unstable> { overlays = [ mitchell-overlay ]; };
in with pkgs.haskell.packages."${compiler}"; {
  markdown-nb-server = markdown-nb-server;
}
