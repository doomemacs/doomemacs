;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-mode)
(package! go-eldoc)
(package! go-guru)
(package! gorepl-mode)

(when (featurep! :completion company)
  (package! company-go))
