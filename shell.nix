let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
    };
  };
};

in

{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz")
  {
    config.allowBroken = false;
    overlays = [ myHaskellPackageOverlay ];
  }
}:

let

  pkgs = nixpkgs;

  haskellDeps = ps: with ps; [
    base mwc-random random
  ];

in

pkgs.stdenv.mkDerivation {
  name = "Whatever";

  buildInputs = [
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
    pkgs.cabal-install
  ];
}
