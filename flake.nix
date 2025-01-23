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

        hs-package-name = "with-utf8";

        ghc-versions = [ "8107" "902" "928" "948" "966" "982" "9101" "9121" ];

        # invoke haskell.nix for each ghc version listed in ghc-versions
        pkgs-per-ghc = lib.genAttrs (map (v: "ghc${v}") ghc-versions)
          (ghc: haskellPkgs.haskell-nix.cabalProject {
            src = haskellPkgs.haskell-nix.haskellLib.cleanGit {
              name = hs-package-name;
              src = ./.;
            };
            compiler-nix-name = ghc;

            # haskell.nix configuration
            modules = [{
              packages.${hs-package-name} = {
                ghcOptions = [
                  # fail on warnings
                  "-Werror"
                  # disable optimisations, we don't need them if we don't package or deploy the executable
                  "-O0"
                ];
              };

            }];
          });

        # returns the list of all components for a package
        get-package-components = pkg:
          # library
          lib.optional (pkg ? library) pkg.library
          # haddock
          ++ lib.optional (pkg ? library) pkg.library.haddock
          # exes, tests and benchmarks
          ++ lib.attrValues pkg.exes
          ++ lib.attrValues pkg.tests
          ++ lib.attrValues pkg.benchmarks;

        # all components for each specified ghc version
        build-all = lib.mapAttrs'
          (ghc: pkg:
            let components = get-package-components pkg.${hs-package-name}.components;
            in lib.nameValuePair "${ghc}:build-all"
              (pkgs.linkFarmFromDrvs "build-all" components)) pkgs-per-ghc;

        # all tests for each specified ghc version
        test-all = lib.mapAttrs'
          (ghc: pkg:
            let tests = lib.filter lib.isDerivation
              (lib.attrValues pkg.${hs-package-name}.checks);
            in lib.nameValuePair "${ghc}:test-all"
              (pkgs.linkFarmFromDrvs "test-all" tests)) pkgs-per-ghc;
      in {
        # nixpkgs revision pinned by this flake
        legacyPackages = pkgs;

        devShells.default = pkgs.mkShell {
            buildInputs = [ pkgs.hpack ];
          };

        # used to dynamically build a matrix in the GitHub pipeline
        ghc-matrix = {
          include = map (ver: { ghc = ver; }) ghc-versions;
        };

        # derivations that we can run from CI
        checks = build-all // test-all // {

          trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;
          reuse-lint = pkgs.build.reuseLint ./.;

          hlint = pkgs.build.haskell.hlint ./.;
          stylish-haskell = pkgs.build.haskell.stylish-haskell ./.;

          hpack = pkgs.build.haskell.hpack ./.;
        };
      });
}
