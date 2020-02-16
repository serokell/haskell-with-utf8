# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{
  edition = 201911;

  description = "Get your UTF-8 IO right on the first try";

  outputs = { self, nixpkgs }:
    {
      packages = {
        haskell-utf8 = null;
      };

      checks = {
        build = self.packages.haskell-utf8;
      };
    };
}
