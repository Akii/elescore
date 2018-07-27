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

  pkgs = import <nixpkgs> { inherit config; };

in
  pkgs.haskellPackages.callPackage ./elescore.nix { }
