;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg)
(when (featurep! :feature eval)
  (package! utop))
(package! merlin)
