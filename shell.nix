{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  nixpkgs = import <nixpkgs> {
    overlays = [
      (self: super: {
        haskellPackages = super.haskellPackages.override {
          overrides = (hself: hsuper: {
            ghc-exactprint = super.haskell.lib.dontCheck (hsuper.ghc-exactprint);
          });
        };
      })
    ];
  };
  inherit (nixpkgs) pkgs;
  drv = import ./default.nix { inherit nixpkgs compiler; };
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv [
    pkgs.cabal-install
    pkgs.hlint
    pkgs.haskellPackages.hindent
    pkgs.haskellPackages.brittany
    pkgs.haskellPackages.ghcid
  ];
in
  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
