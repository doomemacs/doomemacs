;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-eldoc)
(package! go-guru)
(package! go-mode)
(package! gorepl-mode)
(package! go-tag)
(package! go-gen-test)

(when (featurep! :completion company)
  (package! company-go))

(when (featurep! :tools flycheck)
  (package! flycheck-golangci-lint))
