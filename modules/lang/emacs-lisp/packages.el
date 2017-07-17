;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! auto-compile)
(package! highlight-quoted)
(package! slime)
(package! macrostep)

(when (featurep! :feature syntax-checker)
  (package! flycheck-cask))
