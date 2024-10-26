{ pkgs }:
pkgs.lib.pipe
  (pkgs.haskellPackages.callCabal2nix "empty" (pkgs.lib.cleanSource ./.) { })
  [ pkgs.haskell.lib.compose.dontHaddock ]

