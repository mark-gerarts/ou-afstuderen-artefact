{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  yesod-devel = writeScriptBin "yesod-devel" ''
    yesod devel
  '';

in mkShell {

  nativeBuildInputs = [
    haskellPackages.yesod-bin
  ];

  buildInputs = [
    yesod-devel
    stack
    cabal-install
  ];
}
