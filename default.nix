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
    rev = "cc2b6f32a9073748965a4f2e7c874a0acc4c3838";
    sha256 = "05zblr02lkfq83zrrnx88c3y5ras6k5a4dmipv651brd67r4v6qg";
  };

  pkgs = import nixpkgs { inherit config; };

in
  pkgs.haskellPackages.callPackage ./elescore.nix { }
