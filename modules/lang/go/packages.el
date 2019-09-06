;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-eldoc)
(package! go-guru)
(package! go-mode)
(package! gorepl-mode)
(package! go-add-tags)

(when (featurep! :completion company)
  (package! company-go))
