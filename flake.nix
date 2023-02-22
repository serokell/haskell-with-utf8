# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{
  description = "Get your UTF-8 IO right on the first try";

  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    haskell-nix = {
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
    };
    hackage.flake = false;
    stackage.flake = false;
  };

  outputs = { self, flake-utils, serokell-nix, haskell-nix, hackage, stackage, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = haskell-nix.legacyPackages.${system};

        flake =
          serokell-nix.lib.haskell.makeFlake pkgs.haskell-nix pkgs.haskell-nix.stackProject {
            src = ./.;
            ghcVersions = [ "884" "8107" "902" "926" ];
            modules = [ serokell-nix.lib.haskell.ciBuildOptions ];
          };
      in flake // {
          defaultPackage = flake.packages."with-utf8:lib:with-utf8";
          defaultApp = flake.apps."with-utf8:exe:utf8-troubleshoot";
        }
    );
}
