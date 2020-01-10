# Builds a sandbox for Doom Emacs with the latest stable Emacs (26.3). Use this
# as a basis for module shell.nix's.
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

{ pkgs ? (import <nixpkgs> {})
, emacs ? pkgs.emacs
, emacsdir ? "$(pwd)/.."
, doomdir ? "$(pwd)"
, doomlocaldir ? "$(pwd)/.local.nix" }:

pkgs.stdenv.mkDerivation {
  name = "doom-emacs";
  buildInputs = with pkgs; [
    emacs
    git
    (ripgrep.override {withPCRE2 = true;})
  ];
  shellHook = ''
    export EMACSDIR="$(readlink -f "${emacsdir}")/"
    export DOOMDIR="$(readlink -f "${doomdir}")/"
    export DOOMLOCALDIR="$(readlink -f "${doomlocaldir}")/"
    export PATH="$EMACSDIR/bin:$PATH"
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

    doom sync
  '';
}
