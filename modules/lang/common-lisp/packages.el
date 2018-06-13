;; -*- no-byte-compile: t; -*-
;;; lang/common-lisp/packages.el

(package! slime)
(when (featurep! :completion company)
  (package! slime-company))
