{ pkgs ? import <nixpkgs> {}}:

with pkgs;

let
  # Fetch purescript from a specific commit because we want 14.x, and stable
  # only has 13.x.
  pursPin = import (builtins.fetchGit {
      name = "purescript-unstable-pin";
      url = "https://github.com/NixOS/nixpkgs/";
      ref = "refs/heads/nixos-unstable";
      rev = "04a2b269d8921505a2969fc9ec25c1f517f2b307";
  }) {};

  local-hoogle = writeScriptBin "local-hoogle" ''
    ${stack}/bin/stack test --fast --haddock-deps
    ${haskellPackages.hoogle}/bin/hoogle generate --local=./
    ${haskellPackages.hoogle}/bin/hoogle server --local --port=8080
  '';

  serve-backend = writeScriptBin "serve-backend" ''
    ${haskellPackages.yesod-bin}/bin/yesod devel
  '';

  serve-ui = writeScriptBin "serve-ui" ''
    (cd frontend && spago build && npm run serve)
  '';

  serve-both = writeScriptBin "serve-both" ''
    sh -c 'serve-ui & serve-backend'
  '';

  css-build-dev = writeScriptBin "css-build-dev" ''
    ${nodejs}/bin/npm run css-build-dev --prefix=frontend
  '';

  # @todo: stack prod build
  build-prod = writeScriptBin "build-prod" ''
    ${nodejs}/bin/npm run build-prod --prefix=frontend
  '';

  e2e = writeScriptBin "e2e" ''
    ${docker}/bin/docker build . -f Dockerfile.e2e -t e2e-tests && \
    ${docker}/bin/docker run --rm -t e2e-tests
  '';

in mkShell {

  nativeBuildInputs = [
    haskellPackages.yesod-bin
    haskellPackages.hoogle
  ];

  buildInputs = [
    # Scripts
    local-hoogle
    serve-backend
    serve-ui
    serve-both
    build-prod
    css-build-dev
    e2e

    # Backend
    stack
    cabal-install

    # Frontend
    pursPin.purescript
    spago
    nodejs
    nodePackages.parcel-bundler
    nodePackages.purty

    # Tests
    docker

    # Docs
    mkdocs
  ];
}
