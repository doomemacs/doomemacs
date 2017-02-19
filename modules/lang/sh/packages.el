;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (@featurep :completion company)
  (@package company-shell))
