{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  yesod-devel = writeScriptBin "yesod-devel" ''
    ${haskellPackages.yesod-bin}/bin/yesod devel
  '';

  local-hoogle = writeScriptBin "local-hoogle" ''
    ${stack}/bin/stack test --fast --haddock-deps
    ${haskellPackages.hoogle}/bin/hoogle generate --local=./
    ${haskellPackages.hoogle}/bin/hoogle server --local --port=8080
  '';

in mkShell {

  nativeBuildInputs = [
    haskellPackages.yesod-bin
    haskellPackages.hoogle
  ];

  buildInputs = [
    # Scripts
    yesod-devel
    local-hoogle

    stack
    cabal-install
  ];
}
