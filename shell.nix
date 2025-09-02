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
#   nix-shell --argstr emacs 27.1      # 27.1
#   nix-shell --argstr emacs 27.2      # 27.2
#   nix-shell --argstr emacs 27        # 27.2
#   nix-shell --argstr emacs 28.1      # 28.1
#   nix-shell --argstr emacs 28.2      # 28.2
#   nix-shell --argstr emacs 28        # 28.2
#   nix-shell --argstr emacs 29.1      # 29.1
#   nix-shell --argstr emacs 29        # 29.1
#   nix-shell --argstr emacs head      # 30.0.50
#   nix-shell --argstr emacs ci-27.1   # 27.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-27.2   # 27.2    (barebones; no GUI)
#   nix-shell --argstr emacs ci-28.1   # 28.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-28.2   # 28.2    (barebones; no GUI)
#   nix-shell --argstr emacs ci-29.1   # 29.1    (barebones; no GUI)
#   nix-shell --argstr emacs ci-HEAD   # 30.0.50 (barebones; no GUI)

{ sources ? import ./nix/sources.nix, emacs ? "29.1", emacsdir ? "$(pwd)"
, doomdir ? "", doomlocaldir ? "$EMACSDIR/.local", }:
let
  pkgs = import sources.nixpkgs {
    overlays = [
      # emacs-overlay: provides EmacsGit, EmacsNativeComp, and EmacsPgtkNativeComp
      (import sources.emacs-overlay)
      # nix-emacs-ci: provides CI versions of Emacs 27.1, 27.2, 28.1, 28.2, and snapshot
      (_: _: import sources.nix-emacs-ci)
      # emacs{27-{1,2},28-{1,2},29-1}
      (_: _: rec {
        emacs27-1 = (import sources.nixpkgsEmacs27-1 { }).emacs;
        emacs27-2 = (import sources.nixpkgsEmacs27-2 { }).emacs;
        emacs27 = emacs27-2;
        emacs28-1 = (import sources.nixpkgsEmacs28-1 { }).emacs;
        emacs28-2 = (import sources.nixpkgsEmacs28-2 { }).emacs;
        emacs28 = emacs28-2;
        emacs29-1 = (import sources.nixpkgsEmacs29-1 { }).emacs29;
        emacs29 = emacs29-1;
      })
    ];
  };

  inherit (pkgs) lib;

  parsedEmacs = if emacs == "head" then
    "emacsGit"
  else
    "emacs" + lib.replaceStrings [ "ci" "." ] [ "" "-" ] emacs;

  emacsPkg = if builtins.hasAttr parsedEmacs pkgs then
    pkgs."${parsedEmacs}"
  else
    pkgs.emacs;

in pkgs.mkShellNoCC {
  name = "doom-emacs";

  packages = [
    emacsPkg
    pkgs.git
    (pkgs.ripgrep.override { withPCRE2 = true; })
    pkgs.niv
  ];

  EMACS = lib.getExe emacsPkg;
  DOOMNOCOMPILE = 1;

  shellHook = ''
    export EMACSVERSION="$($EMACS --no-site-file --batch --eval '(princ emacs-version)')";

    if [[ -n "${emacsdir}" ]]; then
      export EMACSDIR="$(readlink -f "${emacsdir}")/"
    fi
    if [[ -n "${doomdir}" ]]; then
      export DOOMDIR="$(readlink -f "${doomdir}")/"
    fi
    if [[ -n "${doomlocaldir}" ]]; then
      export DOOMLOCALDIR="$(readlink -f "${doomlocaldir}").$EMACSVERSION/"
    fi

    export PATH="$EMACSDIR/bin:$PATH";

    echo "Running Emacs $EMACSVERSION (emacs=${emacs})"
    echo "EMACSDIR=$EMACSDIR"
    echo "DOOMDIR=$DOOMDIR"
    echo "DOOMLOCALDIR=$DOOMLOCALDIR"

    # Copy your existing repos over to optimize on install times (but not the
    # builds, because that may contain stale bytecode).
    mkdir -p "$DOOMLOCALDIR/straight"
    pushd "$DOOMLOCALDIR/straight" > /dev/null
    if [[ -d "$EMACSDIR/.local/straight/repos" && ! -d ./repos ]]; then
      echo "Copying '$EMACSDIR/.local/straight/repos' to './$(basename $DOOMLOCALDIR)/straight/repos' to save time"
      cp -r "$EMACSDIR/.local/straight/repos" ./repos
    fi
    popd > /dev/null
    echo "Ready! Remember to 'doom sync'!"
  '';
}
