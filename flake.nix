{
  description = "A flake for developing a miso app";

  nixConfig = {
    substituters =
      [ "https://cache.nixos.org/" "https://haskell-miso-cachix.cachix.org" ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "haskell-miso-cachix.cachix.org-1:m8hN1cvFMJtYib4tj+06xkKt5ABMSGfe8W7s40x1kQ0="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-parts.url = "github:hercules-ci/flake-parts";
    miso.url = "github:dmjio/miso/flake-out";
  };

  outputs = inputs@{ self, nixpkgs, haskell-flake, flake-parts, miso, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [ haskell-flake.flakeModule ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.frontend = {
          basePackages = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122;
          # Project root filtering for efficient rebuilds
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              (./. + /frontend)
              (./. + /backend)
              (./. + /shared)
              (./. + /haskell-miso-starter.cabal)
              (./. + /LICENSE)
              (./. + /CHANGELOG.md)
            ];
          });

          # Packages to add on top of `basePackages`
          packages = {
            # Add miso as an override
            miso.source = miso.packages.${system}.default.src;
          };

          # Add your package overrides here
          settings = {
            haskell-miso-starter = {
              # Enable any specific settings for haskell-miso-starter
              check = true;
            };
            miso = {
              # Disable checks for miso to speed up builds
              check = false;
            };
          };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" "devShells" ];
        };

        haskellProjects.default = {
          devShell = {
            tools = hp: {
              fourmolu = hp.fourmolu;
              hlint = hp.hlint;
              ghcid = hp.ghcid;
              haskell-language-server = hp.haskell-language-server;
              cabal-fmt = hp.cabal-fmt;
            };
            hlsCheck.enable = true;
          };
        };

        haskellProjects.backend = {
          basePackages = pkgs.haskell.packages.ghc912;
          # Project root filtering for efficient rebuilds
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              (./. + /backend)
              (./. + /frontend)
              (./. + /shared)
              (./. + /haskell-miso-starter.cabal)
              (./. + /LICENSE)
              (./. + /CHANGELOG.md)
            ];
          });

          # Packages to add on top of `basePackages`
          packages = {
            # Add miso as an override
            miso.source = miso.packages.${system}.default.src;
          };

          # Add your package overrides here
          settings = {
            haskell-miso-starter = {
              # Enable any specific settings for haskell-miso-starter
              check = true;
            };
            miso = {
              # Disable checks for miso to speed up builds
              check = false;
            };
          };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" "devShells" ];
        };

        # Default package & app.
        packages.default = pkgs.writeShellScriptBin "haskell-miso-starter" ''
          export STATIC_DIR=${self'.packages.frontend-haskell-miso-starter}/bin/frontend.jsexe/all.js
          ${self'.packages.backend-haskell-miso-starter}/bin/backend
        '';
      };
    };
}
