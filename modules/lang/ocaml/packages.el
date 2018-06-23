;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg)
(package! merlin)

(cond ((featurep! +lsp)
       (depends-on! :tools lsp)
       (package! lsp-ocaml)))
