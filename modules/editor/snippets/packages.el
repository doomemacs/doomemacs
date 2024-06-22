;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet :pin "eb5ba2664c3a68ae4a53bb38b85418dd131b208f")
(package! auto-yasnippet :pin "6a9e406d0d7f9dfd6dff7647f358cb05a0b1637e")
(package! doom-snippets
  :recipe (:host github
           :repo "doomemacs/snippets"
           :files (:defaults "*"))
  :pin "3a0b029c5d104521ede8dcbeaff2972f56f067dc")
