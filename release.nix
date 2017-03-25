let
  pkgs = import <nixpkgs> {  };

in
  { acid-state = pkgs.haskellPackages.callPackage ./default.nix {}; }
