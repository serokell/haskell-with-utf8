# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{
  edition = 201911;

  description = "Get your UTF-8 IO right on the first try";

  outputs = { self, nixpkgs, haskell-nix }:
    let
      pkgs = nixpkgs {
        overlays = haskell-nix.overlays;
      };

      project = pkgs.haskell-nix.stackProject {
        src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "haskell-utf8";
          src = ./.;
        };
      };
      utf8 = project.utf8;
    in {
      packages = {
        haskell-utf8 = utf8.components.library;
      };

      checks = {
        build = self.packages.haskell-utf8;
        test = utf8.checks.utf8-test;
      };
    };
}
