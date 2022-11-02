{compiler ? "ghc942" }:

let
  pkgs = import <nixpkgs> { };
  pkgs-hs = pkgs.haskell.packages."${compiler}";
in

pkgs-hs.developPackage {
  root = ./.;
  withHoogle = false;
  modifier = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs-hs; [
    cabal-install
  ]);
}
