let pkgs = import <nixpkgs> { };

in pkgs.stdenv.mkDerivation rec {
  name = "clojure";
  buildInputs = with pkgs; [ jdk11 leiningen clojure-lsp ];
}
