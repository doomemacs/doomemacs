;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet)
(package! auto-yasnippet)

(package! emacs-snippets
  :recipe (:fetcher github
           :repo "hlissner/emacs-snippets"
           :files ("*")))
