{
  description = "A flake for developing a miso app";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-parts.url = "github:hercules-ci/flake-parts";
    miso.url = "github:dmjio/miso/flake-out";
  };

  outputs = inputs@{ self, nixpkgs, haskell-flake, flake-parts, miso, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          # Project root filtering for efficient rebuilds
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              (./. + /app)
              (./. + /miso-test.cabal)
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
            miso-test = {
              # Enable any specific settings for miso-test
              check = true;
            };
            miso = {
              # Disable checks for miso to speed up builds
              check = false;
            };
          };

          # Development shell configuration
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

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" "devShells" ];
        };

        # Default package & app.
        packages.default = self'.packages.miso-test;
      };
    };
}
