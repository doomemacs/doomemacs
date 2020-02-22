{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
, stdenv ? pkgs.stdenv
, otp ? pkgs.beam.packages.erlangR22
, ...
}:

{
  # NOTE: Unfortunately this has not been packaged for upstream
  # NOTE: Also unfortunately, the upstream nixpkgs support for building Elixir
  #       projects falls rather short.
  elixir-ls = stdenv.mkDerivation rec {
    name = "elixir-ls";
    version = "unstable-20200222";

    src = pkgs.fetchFromGitHub {
      owner = "elixir-lsp";
      repo = "elixir-ls";
      rev = "1abcebe01e5533260ed38e071598aa927963dfe2";
      sha256 = "0q52q1k3qif087jjchf4jggnbcmppmwghgnnskxx4hzb3f1b9lcz";
      # date = 2020-02-22T07:32:37-10:00;
    };

    buildInputs = with pkgs; [ cacert git ] ++ (with otp; [ erlang elixir ]);

    buildPhase = ''
      runHook preBuild
      export HEX_HOME=$PWD
      export MIX_ENV=prod
      export MIX_HOME=$PWD/.mix
      export REBAR_CACHE_DIR=$PWD/.cache/rebar3
      export REBAR_GLOBAL_CONFIG_DIR=$PWD/.config/rebar3
      mix local.hex --force
      mix local.rebar --force
      mix deps.get --only prod
      mix compile
      runHook postBuild
    '';

    installPhase = ''
      mkdir -p $out/bin $out/lib
      mix elixir_ls.release -o $out/lib
      ln -s $out/lib/debugger.sh $out/lib/language_server.sh $out/bin/
    '';

    meta = {
      description = ''
        A frontend-independent IDE "smartness" server for Elixir. Implements the
        JSON-based "Language Server Protocol" standard and provides debugger support via
        VS Code's debugger protocol.
      '';
      homepage = https://github.com/elixir-lsp/elixir-ls;

      license = stdenv.lib.licenses.asl20;
      maintainers = [];
      # https://github.com/NixOS/nixpkgs/blob/d7266d00b29492bc286dd788d04c3dc4f07390cb/pkgs/development/interpreters/elixir/generic-builder.nix#L75
      platforms = stdenv.lib.platforms.unix;
    };
  };
}
