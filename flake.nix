# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
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

  outputs = { self, nixpkgs, flake-utils, haskell-nix, hackage, stackage }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend haskell-nix.overlay;
        inherit (pkgs) lib;

        src = ./.;
        project = args: pkgs.haskell-nix.stackProject ({ inherit src; } // args);
        with-utf8 = (project {}).with-utf8;
        check = args: (project args).with-utf8.checks.with-utf8-test;
      in rec {
        packages = {
          with-utf8 = with-utf8.components.library;
        };
        defaultPackage = packages.with-utf8;

        checks =
          let
            mkGhcCheck = ghcVersion: {
              name = "test-ghc${ghcVersion}";
              value = check { ghc = pkgs.haskell-nix.compiler."ghc${ghcVersion}"; };
            };
          in {
            test = with-utf8.checks.with-utf8-test;
          } // lib.listToAttrs (map mkGhcCheck [ "884" "8107" "901" ]);

        apps.utf8-troubleshoot = {
          type = "app";
          program = "${with-utf8.components.exes.utf8-troubleshoot}/bin/utf8-troubleshoot";
        };
        defaultApp = apps.utf8-troubleshoot;
      }
    );
}
