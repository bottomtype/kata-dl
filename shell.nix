let
  pkgs = import <nixpkgs> { };
  default = import ./default.nix {  };
in
default.overrideAttrs (oldAttrs: rec {
  returnShellEnv = true;
})
