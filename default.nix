let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          datetime = pkgs.haskell.lib.dontCheck haskellPackagesOld.datetime;
        };
      };
    };
  };

  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "b5ca7fefc45ecec8695685d573db39612e26ae87";
    sha256 = "0sdsq7iysjrgwpvacpwzrlfnb3xvfi3b93n78gj4h69mkn71siak";
  };

  pkgs = import nixpkgs { inherit config; };

in
  pkgs.haskellPackages.callPackage ./elescore.nix { }
