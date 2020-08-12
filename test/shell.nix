# Builds a sandbox for Emacs (and optionally, Doom) with a particular version of
# Emacs. Use this as a basis for module shell.nix's.
#
# Usage examples:
#
# To create a doom environment with the test config:
#
#   nix-shell
#
# With your own DOOMDIR:
#
#   nix-shell --argstr doomdir ~/.config/doom
#
# With a specific version of Emacs
#
#   nix-shell --arg emacs 26   # 26.3
#   nix-shell --arg emacs 27   # 27.1
#   nix-shell --arg emacs 28   # HEAD

{ pkgs ?
  import <nixpkgs> {
    overlays = [
      import (builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz)
    ];
  }
, emacs ? 26
, emacsdir ? "$(pwd)/.."
, doomdir ? "$(pwd)"
, doomlocaldir ? "$(pwd)/.local" }:

pkgs.stdenv.mkDerivation {
  name = "doom-emacs";

  buildInputs = [
    (if      emacs == 28 then pkgs.emacsGit
     else if emacs == 27 then pkgs.emacsUnstable
     else pkgs.emacs)
    pkgs.git
    (pkgs.ripgrep.override {withPCRE2 = true;})
  ];

  shellHook = ''
    export EMACSVERSION="$(emacs --no-site-file --batch --eval '(princ emacs-version)')"
    export EMACSDIR="$(readlink -f "${emacsdir}")/"
    export DOOMDIR="$(readlink -f "${doomdir}")/"
    export DOOMLOCALDIR="$(readlink -f "${doomlocaldir}").$EMACSVERSION/"
    export PATH="$EMACSDIR/bin:$PATH"

    echo "Running Emacs $EMACSVERSION"
    echo "EMACSDIR=$EMACSDIR"
    echo "DOOMDIR=$DOOMDIR"
    echo "DOOMLOCALDIR=$DOOMLOCALDIR"

    # Copy your existing repos over to optimize on install times (but not the
    # builds, because that may contain stale bytecode).
    mkdir -p "$DOOMLOCALDIR/straight"
    pushd "$DOOMLOCALDIR/straight" >/dev/null
    if [[ -d "$EMACSDIR/.local/straight/repos" && ! -d ./repos ]]; then
      cp -r "$EMACSDIR/.local/straight/repos" ./repos
    fi
    popd >/dev/null
  '';
}
