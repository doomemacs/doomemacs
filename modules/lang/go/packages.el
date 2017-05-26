;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-eldoc)
(package! go-guru)
(package! go-mode)
(package! gorepl-mode)

(when (featurep! :completion company)
  (package! company-go))
