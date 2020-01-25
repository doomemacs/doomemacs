;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (featurep! :completion company)
  (package! company-shell))

(when (featurep! +fish)
  (package! fish-mode :pin "688c82deca"))
