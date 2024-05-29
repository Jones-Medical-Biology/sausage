{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    ghcid
    stack
    emacs-gtk
    haskellPackages.cabal-install
  ];
}
