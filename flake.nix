# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{
  description = "Get your UTF-8 IO right on the first try";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskell-nix.url = "github:input-output-hk/haskell.nix/ba87ffb84bc4158ada14908bddee9df2c81ac31b";
    nixpkgs.url = "github:nixos/nixpkgs/f02bf8ffb9a5ec5e8f6f66f1e5544fd2aa1a0693";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend haskell-nix.overlay;

        project = pkgs.haskell-nix.stackProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "with-utf8";
            src = ./.;
          };
        };
        with-utf8 = project.with-utf8;
      in rec {
        packages = {
          with-utf8 = with-utf8.components.library;
        };
        defaultPackage = packages.with-utf8;

        checks = {
          build = packages.with-utf8;
          test = with-utf8.checks.with-utf8-test;
        };

        apps.utf8-troubleshoot = {
          type = "app";
          program = "${with-utf8.components.exes.utf8-troubleshoot}/bin/utf8-troubleshoot";
        };
        defaultApp = apps.utf8-troubleshoot;
      }
    );
}
