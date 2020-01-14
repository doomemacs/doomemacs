;; -*- no-byte-compile: t; -*-
;;; lang/crystal/packages.el

(package! crystal-mode)
(package! inf-crystal)
(when (featurep! :checkers syntax)
  (package! flycheck-crystal)
  (package! flycheck-ameba))
