# haskell-miso-starter

This repo should give an easy way to build a full stack web app with [haskell-miso.org](https://haskell-miso.org), by providing a flake.nix.
The flake should build the software and provide a development environment. 

This repo is under development and several things dont work right now. See Notes/Todos.

The starter haskell code is partially copied from: https://github.com/dmjio/miso/tree/master/haskell-miso.org

## Commands you can use:
- nix develop -> this gives you a development shell with a HLS, cabal, and some tool
- nix build -> this build the software and should result in an executable binary at ./result/bin/


## Notes:
- right now there is now cached version of pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122, thats why the first build takes really long (45 min)
    -> is there a ghcjs version which is cached

## Todo:
- client site routing does not work and it always makes a complete page refresh
- uriHome not correct in navigation - why?