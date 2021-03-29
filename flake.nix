# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{
  description = "Get your UTF-8 IO right on the first try";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskell-nix.url = "github:input-output-hk/haskell.nix/43cb0fc8957be7ab027f8bd5d48bc22479032c1f";
    nixpkgs.url = "github:serokell/nixpkgs/25cb6c920e31f80cc4c4559c840c5753d4a9012f";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:
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
            mkGhcCheck = ghcName: {
              name = "test-${ghcName}";
              value = check { ghc = pkgs.haskell-nix.compiler.${ghcName}; };
            };
          in {
            test = with-utf8.checks.with-utf8-test;
          } // lib.listToAttrs (map mkGhcCheck [ "ghc884" "ghc8104" ]);

        apps.utf8-troubleshoot = {
          type = "app";
          program = "${with-utf8.components.exes.utf8-troubleshoot}/bin/utf8-troubleshoot";
        };
        defaultApp = apps.utf8-troubleshoot;
      }
    );
}
