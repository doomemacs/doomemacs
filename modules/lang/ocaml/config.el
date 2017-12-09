;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

(def-package! tuareg
  :mode ("\\.ml[4ilpy]?$" . tuareg-mode))


(def-package! merlin
  :after tuareg
  :hook (tuareg-mode . merlin-mode))
