;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

(package! highlight-quoted)
(package! macrostep)
(package! overseer)
(package! elisp-def)
(package! elisp-demos)
(package! emr)

(when (featurep! :tools flycheck)
  (package! flycheck-cask))

(package! buttercup)
