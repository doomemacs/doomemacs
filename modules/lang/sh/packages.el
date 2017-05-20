;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

;; requires shellcheck
;; optional: zshdb bashdb

(when (featurep! :completion company)
  (package! company-shell))
