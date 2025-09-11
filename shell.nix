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
#   nix-shell --argstr emacs 26.1
#   nix-shell --argstr emacs 26.2
#   nix-shell --argstr emacs 26.3
#   nix-shell --argstr emacs 26        # 26.3
#   nix-shell --argstr emacs 27.1
#   nix-shell --argstr emacs 27.2
#   nix-shell --argstr emacs 27        # 27
#   nix-shell --argstr emacs 28.1
#   nix-shell --argstr emacs 28.2
#   nix-shell --argstr emacs 28        # 28
#   nix-shell --argstr emacs 29.1
#   nix-shell --argstr emacs 29.2
#   nix-shell --argstr emacs 29.3
#   nix-shell --argstr emacs 29.4
#   nix-shell --argstr emacs 29        # 29.4
#   nix-shell --argstr emacs 30.1
#   nix-shell --argstr emacs 30.2
#   nix-shell --argstr emacs 30        # 30.2
#   nix-shell --argstr emacs head      # 31.0.50
#   nix-shell --argstr emacs ci-26.3   # 26.3    (barebones; no GUI)
#   nix-shell --argstr emacs ci-27.1   # 27.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-27.2   # 27.2    (barebones; no GUI)
#   nix-shell --argstr emacs ci-28.1   # 28.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-28.2   # 28.2    (barebones; no GUI)
#   nix-shell --argstr emacs ci-29.1   # 29.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-HEAD   # 30.0.50 (barebones; no GUI)

{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/24.11.tar.gz")
  {
    overlays = [
      # emacs-overlay: provides EmacsGit, EmacsNativeComp, and EmacsPgtkNativeComp
      (import (fetchTarball "https://github.com/nix-community/emacs-overlay/archive/88e410d7f1ddef554b40f66755626e5c883487d9.tar.gz"))
      # nix-emacs-ci: provides CI versions of Emacs 26.3, 27.1, 27.2, 28.1, 28.2, and snapshot
      (_: _: (import (fetchTarball "https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz")))
    ];
  }
, emacs ? "30.2"
, emacsdir ? "$(pwd)"
, doomdir ? ""
, doomlocaldir ? "$EMACSDIR/.local" }:

let getNixpkgs = rev: import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
    emacsPkg = (rec {
                  "26.1" = (getNixpkgs "19.03").emacs26;
                  "26.2" = (getNixpkgs "2255f292063ccbe184ff8f9b35ce475c04d5ae69").emacs26;
                  "26.3" = (getNixpkgs "20.09").emacs26;
                  "26"   = (getNixpkgs "20.09").emacs26;
                  "27.1" = (getNixpkgs "20.09").emacs27;
                  "27.2" = (getNixpkgs "21.11").emacs27;
                  "27"   = (getNixpkgs "21.11").emacs27;
                  "28.1" = (getNixpkgs "22.05").emacs28;
                  "28.2" = (getNixpkgs "24.11").emacs28;
                  "28"   = (getNixpkgs "24.11").emacs28;
                  "29.1" = (getNixpkgs "23.11").emacs29;
                  "29.2" = (getNixpkgs "d3635821355bd04098e8139145eecdcaa30c1d4e").emacs29;
                  "29.3" = (getNixpkgs "24.05").emacs29;
                  "29.4" = (getNixpkgs "24.11").emacs29;
                  "29"   = (getNixpkgs "24.11").emacs29;
                  "30.1" = (getNixpkgs "25.05").emacs;
                  "30.2" = (getNixpkgs "nixos-25.05-small").emacs;
                  "30"   = (getNixpkgs "nixos-25.05-small").emacs;
                  "head" = pkgs.emacsGit;
                  "ci-26.1" = pkgs.emacs-26-1;
                  "ci-26.2" = pkgs.emacs-26-2;
                  "ci-26.3" = pkgs.emacs-26-3;
                  "ci-27.1" = pkgs.emacs-27-1;
                  "ci-27.2" = pkgs.emacs-27-2;
                  "ci-28.1" = pkgs.emacs-28-1;
                  "ci-28.2" = pkgs.emacs-28-2;
                  "ci-29.1" = pkgs.emacs-29-1;
                  "ci-29.2" = pkgs.emacs-29-2;
                  "ci-29.3" = pkgs.emacs-29-3;
                  "ci-30.1" = pkgs.emacs-30-1;
                  "ci-30.2" = pkgs.emacs-30-2;
                  "ci-head" = pkgs.emacs-snapshot;
                })."${emacs}";
in pkgs.stdenv.mkDerivation {
  name = "doomemacs";

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
