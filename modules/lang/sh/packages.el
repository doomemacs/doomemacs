;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (featurep! :completion company)
  (package! company-shell :pin "52f3bf26b7"))

(when (featurep! +fish)
  (package! fish-mode :pin "db257db810"))
