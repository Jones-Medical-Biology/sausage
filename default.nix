{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    ghcid
    stack
    emacs-gtk
    haskellPackages.hpack
    haskellPackages.cabal-install
    haskellPackages.parsec
    haskell-language-server
  ];
}
