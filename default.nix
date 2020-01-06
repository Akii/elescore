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
    rev = "c5c2ac4f45959727b043b3478f28829fac36fffb";
    sha256 = "05zblr02lkfq83zrrnx88c3y5ras6k5a4dmipv651brd67r4v6qg";
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  pkgs.haskellPackages.callPackage ./elescore.nix { }
