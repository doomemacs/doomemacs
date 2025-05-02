;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet :pin "2384fe1655c60e803521ba59a34c0a7e48a25d06")
(package! auto-yasnippet :pin "6a9e406d0d7f9dfd6dff7647f358cb05a0b1637e")
(package! doom-snippets
  :recipe (:host github
           :repo "doomemacs/snippets"
           :files (:defaults "*"))
  :pin "fd4edaaf0c8476a26994db17d084b36733c635e2")
