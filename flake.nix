# SPDX-FileCopyrightText: 2023 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0
{
  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      flake = false;
    };
    haskell-nix = {
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
    };
    serokell-nix.url = "github:serokell/serokell.nix/sereja/OPS-1458-ci-wrapper";
    hackage = {
      flake = false;
    };
    stackage = {
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, hackage, stackage, serokell-nix, flake-compat, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        haskellPkgs = haskell-nix.legacyPackages."${system}";
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            serokell-nix.overlay
          ];
        };

        lib = pkgs.lib;

       ci = serokell-nix.lib.haskell.makeCI haskellPkgs {
          src = ./.;
          stackFiles = ["stack-lts-20-7.yaml"];
          resolvers = ["lts-19.13"];
        };

      in {
        # nixpkgs revision pinned by this flake
        legacyPackages = pkgs;

        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.hpack ];
        };

        # used to dynamically build a matrix in the GitHub pipeline
        inherit (ci) build-matrix ;

        # derivations that we can run from CI
        checks = ci.build-all // ci.test-all // {

          # trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;
          # reuse-lint = pkgs.build.reuseLint ./.;

          hlint = pkgs.build.haskell.hlint ./.;
          stylish-haskell = pkgs.build.haskell.stylish-haskell ./.;

          hpack = pkgs.build.haskell.hpack ./.;
        };
      });
}
