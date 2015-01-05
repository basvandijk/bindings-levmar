let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./bindings-levmar.nix {}
