# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{
  description = "Get your UTF-8 IO right on the first try";

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    serokell-nix.url = "github:serokell/serokell.nix/kirelagin/lib.haskell.makeFlake";
    nixpkgs.url = "github:serokell/nixpkgs";
    haskell-nix = {
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
    };
    hackage.flake = false;
    stackage.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, serokell-nix, haskell-nix, hackage, stackage }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend haskell-nix.overlay;

        flake =
          serokell-nix.lib.haskell.makeFlake pkgs.haskell-nix pkgs.haskell-nix.stackProject {
            src = ./.;
            ghcVersions = [ "884" "8107" "901" ];
          };
      in flake // {
          defaultPackage = flake.packages."with-utf8:lib:with-utf8";
          defaultApp = flake.apps."with-utf8:exe:utf8-troubleshoot";
        }
    );
}
