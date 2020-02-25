# Builds an sandbox for Doom Emacs with Emacs HEAD (27/28). Warning: it compiles
# Emacs; this takes a while!
#
# To create a doom environment w/ Emacs 27/28 with the test config:
#
#   nix-shell shell.head.nix
#
# Or from a custom DOOMDIR:
#
#   nix-shell --argstr doomdir ~/.config/doom

{ emacsdir ? "$(pwd)/../"
, doomdir ? "$(pwd)"
, doomlocaldir ? "$(pwd)/.local.nix.head" }:

import ./shell.nix rec {
  pkgs = import <nixpkgs> {
    overlays = [
      (import (builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz))
    ];
  };
  emacs = pkgs.emacsGit;
  inherit emacsdir doomdir doomlocaldir;
}
