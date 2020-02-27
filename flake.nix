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
          name = "with-utf8";
          src = ./.;
        };
      };
      with-utf8 = project.with-utf8;
    in {
      packages = {
        with-utf8 = with-utf8.components.library;
      };

      checks = {
        build = self.packages.with-utf8;
        test = with-utf8.checks.with-utf8-test;
      };
    };
}
