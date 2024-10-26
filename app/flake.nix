{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "empty";
        pkgs = import nixpkgs { inherit system; };

        package = pkgs.lib.pipe
          (pkgs.haskellPackages.callCabal2nix name (pkgs.lib.cleanSource ./.) { })
          [ hlib.dontHaddock ];

        myHaskellPackages = pkgs.haskellPackages.extend
          (final: prev: { ${name} = package; });

        nativeBuildInputs = with pkgs; [ ghc cabal-install ];
        hlib = pkgs.haskell.lib.compose;
      in
      rec {
        devShell = myHaskellPackages.shellFor
          {
            packages = p: [ p.${name} ];
            nativeBuildInputs = with pkgs;
              nativeBuildInputs ++ [
                niv
                hlint
                ormolu
                (ghc.withPackages (p: [ p.haskell-language-server ]))
              ];
          };

        packages.${name} = package;
        packages.default = packages.${name};
        defaultPackage = packages.${name};

        apps.default = flake-utils.lib.mkApp { drv = packages.default; };
      }
    );
}
