;; -*- no-byte-compile: t; -*-
;;; completion/lsp-bridge/packages.el

(when (package! lsp-bridge
        :pin "3b37a04bd1b6bbcdc2b0ad7a5c388ad027eb7a25"
        :recipe (:host github
                 :repo "manateelazycat/lsp-bridge"
                 :branch "master"
                 :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                 ;; do not perform byte compilation or native compilation for lsp-bridge
                 :build (:not compile)))
  (package! markdown-mode)
  (package! yasnippet))
