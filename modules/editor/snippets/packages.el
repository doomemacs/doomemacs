;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet :pin "33587a8551b8f6991b607d3532062a384c010ce1")
(package! auto-yasnippet :pin "6a9e406d0d7f9dfd6dff7647f358cb05a0b1637e")
(package! doom-snippets
  :recipe (:host github
           :repo "doomemacs/snippets"
           :files (:defaults "*"))
  :pin "07b3bdaf60b74080f899b12c3d46594c7fa75e04")
