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
#   nix-shell --argstr emacs 26        # 26.3
#   nix-shell --argstr emacs 26.3      # 26.3
#   nix-shell --argstr emacs 27        # 27.1
#   nix-shell --argstr emacs 27.1      # 27.1
#   nix-shell --argstr emacs 27.2      # 27.2
#   nix-shell --argstr emacs 28        # 28.0.50
#   nix-shell --argstr emacs gcc       # 28.0.50 + native-comp
#   nix-shell --argstr emacs pgtk+gcc  # 28.0.50 + pgtk + native-comp
#   nix-shell --argstr emacs ci-26.3   # 26.3    (barebones; no GUI)
#   nix-shell --argstr emacs ci-27.1   # 27.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-27.2   # 27.2    (barebones; no GUI)
#   nix-shell --argstr emacs ci-HEAD   # 28.0.50 (barebones; no GUI)

# nixpkgs: provides Emacs 26.3 and 27.1
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz")
  {
    overlays = [
      # emacs-overlay: provides Emacs 28, native-comp, and pgtk
      (import (fetchTarball "https://github.com/nix-community/emacs-overlay/archive/85cc3a1fd067440181a4902455912e7c08ec26c8.tar.gz"))
      # nix-emacs-ci: provides CI versions of Emacs 26.3, 27.1, 27.2, and 28.0.50
      (_: _: (import (fetchTarball "https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz")))
      # Pull Emacs 27.2 from later version of nixpkgs
      (_: _: { emacs27-2 = (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4c87cb87a2db6b9eb43541c1cf83f2a2f725fa25.tar.gz") {}).emacs; })
    ];
  }
, emacs ? "27"
, emacsdir ? "$(pwd)/.."
, doomdir ? "$(pwd)"
, doomlocaldir ? "$(pwd)/.local" }:

let emacsPkg = (if      emacs == "26"       then pkgs.emacs26
                else if emacs == "26.3"     then pkgs.emacs26
                else if emacs == "27"       then pkgs.emacs27-2
                else if emacs == "27.1"     then pkgs.emacs27
                else if emacs == "27.2"     then pkgs.emacs27-2
                else if emacs == "28"       then pkgs.emacsGit
                else if emacs == "gcc"      then pkgs.emacsGcc
                else if emacs == "pgtk+gcc" then pkgs.emacsPgtkGcc
                else if emacs == "ci-26.3"  then pkgs.emacs-26-3
                else if emacs == "ci-27.1"  then pkgs.emacs-27-1
                else if emacs == "ci-27.2"  then pkgs.emacs-27-2
                else if emacs == "ci-28"    then pkgs.emacs-snapshot
                else pkgs.emacs);
in pkgs.stdenv.mkDerivation {
  name = "doom-emacs";

  buildInputs = [
    emacsPkg
    pkgs.git
    (pkgs.ripgrep.override {withPCRE2 = true;})
  ];

  shellHook = ''
    export EMACS="${emacsPkg}/bin/emacs"
    export EMACSVERSION="$($EMACS --no-site-file --batch --eval '(princ emacs-version)')"
    export EMACSDIR="$(readlink -f "${emacsdir}")/"
    export DOOMDIR="$(readlink -f "${doomdir}")/"
    export DOOMLOCALDIR="$(readlink -f "${doomlocaldir}").$EMACSVERSION/"
    export DOOMNOCOMPILE=1
    export PATH="$EMACSDIR/bin:$PATH"

    echo "Running Emacs $EMACSVERSION (emacs=${emacs})"
    echo "EMACSDIR=$EMACSDIR"
    echo "DOOMDIR=$DOOMDIR"
    echo "DOOMLOCALDIR=$DOOMLOCALDIR"

    # Copy your existing repos over to optimize on install times (but not the
    # builds, because that may contain stale bytecode).
    mkdir -p "$DOOMLOCALDIR/straight"
    pushd "$DOOMLOCALDIR/straight" >/dev/null
    if [[ -d "$EMACSDIR/.local/straight/repos" && ! -d ./repos ]]; then
      echo "Copying '$EMACSDIR/.local/straight/repos' to './$(basename $DOOMLOCALDIR)straight/repos' to save time"
      cp -r "$EMACSDIR/.local/straight/repos" ./repos
    fi
    popd >/dev/null
    echo "Ready!"
  '';
}
