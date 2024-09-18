let
  pkgsUrl = "https://github.com/NixOS/nixpkgs/archive/7e1f4f7daf9be03f9fe94b80b27054eddbcddb23.tar.gz";
in
{ pkgs ? import (fetchTarball pkgsUrl) { } }:

let
  ghcVer = "ghc965";
in
{
  build = (pkgs.haskell.packages.${ghcVer}.callPackage ./bf.nix { });

  shell = pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      stack
      cabal-install
      # haskell-language-server
      cabal2nix

      (haskell-language-server.override {
        supportedGhcVersions = [ "965" ];
      })
      stylish-haskell
    ];

    buildInputs = with pkgs; [
      haskell.compiler.${ghcVer}
    ];
  };
}

