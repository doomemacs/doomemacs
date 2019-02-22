;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! auto-compile)
(package! highlight-quoted)
(package! macrostep)
(package! overseer)
(package! elisp-def)

(when (featurep! :tools flycheck)
  (package! flycheck-cask))
