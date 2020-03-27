;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet :pin "ac03c2f192")
(package! auto-yasnippet :pin "db9e0dd433")
(package! doom-snippets
  :recipe (:host github
           :repo "hlissner/doom-snippets"
           :files ("*.el" "*"))
  :pin "2a0c3cf901")
