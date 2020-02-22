# Example usage:
# nix-shell --pure --command 'edit-sample-app' --arg doomtty false
#
# Example keystrokes:
# C-c a t       Run Tests
# SPC c F       LSP format buffer
# K             View documentation for module/function under point

{ pkgs ? (import <nixpkgs> {})
, emacs ? pkgs.emacs
, emacsdir ? "$(pwd)/../../.."
, doomdir ? null
, doomlocaldir ? "$(pwd)/.local.nix"
, doomdebug ? false
, doomtty ? false
, extraconfig ? ""
, lsp ? true
, modulename ? "elixir"
, moduledecl ? if lsp then "(${modulename} +lsp)" else modulename
, sampledir ? "$(pwd)/sample"
, otp ? pkgs.beam.packages.erlangR22
}:

let
  inherit (otp) elixir erlang;

  doomInit = pkgs.writeTextDir ".config/doom/init.el" ''
    (doom! :completion
          company

          :ui
          doom
          modeline

          :editor
          (evil +everywhere)
          snippets

          :checkers
          syntax

          :tools
          eval
          lookup
          lsp

          :lang
          ${moduledecl}

          :config
          (default +bindings +smartparens))
  '';

  lspConfig = if lsp then ''
    (after! elixir
      (add-hook! elixir-mode #'lsp))

    ;; https://elixirforum.com/t/emacs-elixir-setup-configuration-wiki/19196
    ;; Heading: ElixirLS Configuration
    (defvar lsp-elixir--config-options (make-hash-table))
    (puthash "dialyzerEnabled" :json-false lsp-elixir--config-options)

    (after! lsp
      (setq lsp-enable-file-watchers nil)
      (add-hook 'lsp-after-initialize-hook
        (lambda ()
          (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options)))))
  '' else "";

  # TODO: Upstream add-hook! into module itself?
  doomConfig = pkgs.writeTextDir ".config/doom/config.el" ''
    ${lspConfig}
    ${extraconfig}
  '';

  doomPackages = pkgs.writeTextDir ".config/doom/packages.el" ''
  '';

  doomDir = pkgs.symlinkJoin {
    name = "doomdir-${modulename}-module";
    paths = [ doomConfig doomInit doomPackages ];
  };

  create-sample-app = pkgs.writeShellScriptBin "create-sample-app" ''
    test -d ${sampledir} || ${elixir}/bin/mix new ${sampledir} --sup --app sample
  '';

  edit-sample-app = pkgs.writeShellScriptBin "edit-sample-app" ''
    create-sample-app
    ${emacs}/bin/emacs ${if doomtty then "-nw" else ""} ${sampledir}/mix.exs
  '';

  validate-sample-app = pkgs.writeShellScriptBin "validate-sample-app" ''
    pushd ${sampledir} >/dev/null
    ${elixir}/bin/elixir -S mix test
    popd >/dev/null
  '';

  inherit (import ./default.nix { inherit pkgs otp; }) elixir-ls;
in
pkgs.stdenv.mkDerivation {
  name = "doom-emacs";
  buildInputs = with pkgs; [
    emacs
    git
    (ripgrep.override { withPCRE2 = true; })
  ] ++ [
    # derivation arguments
    elixir
    erlang

    # local scripts
    create-sample-app
    edit-sample-app
    validate-sample-app
  ] ++ (
    with otp; [
      hex
      rebar
      rebar3
    ]
  ) ++ lib.optional (lsp) elixir-ls;

  shellHook = ''
    export EMACSDIR="$(readlink -f "${emacsdir}")/"
    export DOOMDIR="${doomDir}/.config/doom"
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

    doom ${if doomdebug then "-d" else ""} sync
  '';
}
