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
#   nix-shell --argstr doomdir ~/.config/other-doom-config
#
# With a specific version of Emacs
#
#   nix-shell --argstr emacs 26.3      # 26.3
#   nix-shell --argstr emacs 27.1      # 27.1
#   nix-shell --argstr emacs 27.2      # 27.2
#   nix-shell --argstr emacs 28.1      # 28.1
#   nix-shell --argstr emacs 28.2      # 28.2
#   nix-shell --argstr emacs 29.1      # 29.1
#   nix-shell --argstr emacs head      # 30.0.50
#   nix-shell --argstr emacs ci-26.3   # 26.3    (barebones; no GUI)
#   nix-shell --argstr emacs ci-27.1   # 27.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-27.2   # 27.2    (barebones; no GUI)
#   nix-shell --argstr emacs ci-28.1   # 28.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-28.2   # 28.2    (barebones; no GUI)
#   nix-shell --argstr emacs ci-29.1   # 29.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-HEAD   # 30.0.50 (barebones; no GUI)

{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/22.05.tar.gz")
  {
    overlays = [
      # emacs-overlay: provides EmacsGit, EmacsNativeComp, and EmacsPgtkNativeComp
      (import (fetchTarball "https://github.com/nix-community/emacs-overlay/archive/88e410d7f1ddef554b40f66755626e5c883487d9.tar.gz"))
      # nix-emacs-ci: provides CI versions of Emacs 26.3, 27.1, 27.2, 28.1, 28.2, and snapshot
      (_: _: (import (fetchTarball "https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz")))
      # emacs{26,27-{1,2},28-1}
      (_: _: {
        emacs26   = (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/11264a390b197b80edeffac6f20e582f3ea318bd.tar.gz") {}).emacs26;
        emacs27-1 = (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/11264a390b197b80edeffac6f20e582f3ea318bd.tar.gz") {}).emacs;
        emacs27-2 = (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4c87cb87a2db6b9eb43541c1cf83f2a2f725fa25.tar.gz") {}).emacs;
        emacs28-1 = (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/54e795706d9fa90004ed1274af639a717f04a2aa.tar.gz") {}).emacs;
        emacs29-1 = (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/c434383f2a4866a7c674019b4cdcbfc55db3c4ab.tar.gz") {}).emacs29;
      })
    ];
  }
, emacs ? "29.1"
, emacsdir ? "$(pwd)"
, doomdir ? ""
, doomlocaldir ? "$EMACSDIR/.local" }:

let emacsPkg = (if      emacs == "26.3"     then pkgs.emacs26
                else if emacs == "27.1"     then pkgs.emacs27-1
                else if emacs == "27.2"     then pkgs.emacs27-2
                else if emacs == "28.1"     then pkgs.emacs28-1
                else if emacs == "28.2"     then pkgs.emacs
                else if emacs == "29.1"     then pkgs.emacs29-1
                else if emacs == "head"     then pkgs.emacsGit
                else if emacs == "ci-26.3"  then pkgs.emacs-26-3
                else if emacs == "ci-27.1"  then pkgs.emacs-27-1
                else if emacs == "ci-27.2"  then pkgs.emacs-27-2
                else if emacs == "ci-28.1"  then pkgs.emacs-28-1
                else if emacs == "ci-28.2"  then pkgs.emacs-28-2
                else if emacs == "ci-29.1"  then pkgs.emacs-29-1
                else if emacs == "ci-head"  then pkgs.emacs-snapshot
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
    if [[ -n "${emacsdir}" ]]; then
      export EMACSDIR="$(readlink -f "${emacsdir}")/"
    fi
    if [[ -n "${doomdir}" ]]; then
      export DOOMDIR="$(readlink -f "${doomdir}")/"
    fi
    if [[ -n "${doomlocaldir}" ]]; then
      export DOOMLOCALDIR="$(readlink -f "${doomlocaldir}").$EMACSVERSION/"
    fi
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
      echo "Copying '$EMACSDIR/.local/straight/repos' to './$(basename $DOOMLOCALDIR)/straight/repos' to save time"
      cp -r "$EMACSDIR/.local/straight/repos" ./repos
    fi
    popd >/dev/null
    echo "Ready! Remember to 'doom sync'!"
  '';
}
