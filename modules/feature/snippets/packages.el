;; -*- no-byte-compile: t; -*-
;;; feature/snippets/packages.el

(package! yasnippet)
(package! auto-yasnippet)

(package! emacs-snippets
  :recipe (:fetcher github
           :repo "hlissner/emacs-snippets"
           :files ("*")))
