{ nixpkgs ? import <nixpkgs> { } }:

let
  inherit (nixpkgs) pkgs;

  doom-emacs = pkgs.callPackage ../../../test/shell.nix { };
  clojure = import ./test/clojure.nix;

in pkgs.stdenv.mkDerivation {
  name = "clojure-shell";
  buildInputs = clojure.buildInputs ++ doom-emacs.buildInputs;
  shellHook = doom-emacs.shellHook;
}
