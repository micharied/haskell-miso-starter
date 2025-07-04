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

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" "devShells" ];
        };

        # Default package & app.
        packages.default = pkgs.writeShellScriptBin "haskell-miso-starter" ''
              export STATIC_DIR=${self'.packages.frontend-miso-test}/bin/frontend.jsexe/all.js
              ${self'.packages.backend-miso-test}/bin/backend
            '';

        packages.defasssult = pkgs.runCommand "haskell-miso-starter" { } ''
                    mkdir -p $out/{bin,static}
                    cp ${self'.packages.backend-miso-test}/bin/* $out/bin/
                    cp -v ${self'.packages.frontend-miso-test}/bin/frontend.jsexe/all.js $out/static/all.js
                    
                    # Create a wrapper script that sets STATIC_DIR
                    cat > $out/bin/run-backend << 'EOF'
          #!/usr/bin/env bash
          export STATIC_DIR="$(dirname "$0")/../static"
          exec "$(dirname "$0")/backend" "$@"
          EOF
                    chmod +x $out/bin/run-backend
        '';
      };
    };
}
