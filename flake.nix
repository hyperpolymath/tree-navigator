# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
# tree-navigator - Nix Flake (fallback when Guix unavailable)
# Usage: nix develop  OR  nix build
{
  description = "Type-safe directory tree navigator written in Ada 2022";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "tree-navigator";
          version = "0.1.0";

          src = ./.;

          nativeBuildInputs = with pkgs; [
            gnat
            gprbuild
          ];

          buildInputs = with pkgs; [
            cacert  # For HTTPS certificate validation
          ];

          buildPhase = ''
            gprbuild -P tree_navigator.gpr -j$NIX_BUILD_CORES -p
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp bin/main $out/bin/tree-navigator
          '';

          meta = with pkgs.lib; {
            description = "Type-safe directory tree export and navigation tool";
            homepage = "https://github.com/hyperpolymath/tree-navigator";
            license = with licenses; [ mit agpl3Plus ];
            maintainers = [ ];
            platforms = platforms.unix;
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gnat
            gprbuild
            just
            git
          ];

          shellHook = ''
            echo "🌲 Tree Navigator development shell"
            echo "Build: gprbuild -P tree_navigator.gpr"
            echo "Or:    just build"
          '';
        };
      }
    );
}
