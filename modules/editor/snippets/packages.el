;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet :pin "297546f0853a6a51f5b05e954d0c6aea8caa5ec2")
(package! auto-yasnippet :pin "6a9e406d0d7f9dfd6dff7647f358cb05a0b1637e")
(package! doom-snippets
  :recipe (:host github
           :repo "doomemacs/snippets"
           :files (:defaults "*"))
  :pin "f022984ee1318a4015d5d081b3c3dab5a60dc6ff")
