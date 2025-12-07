;; -*- no-byte-compile: t; -*-
;;; core/use-package/packages.el

(package! bind-key
  ;; HACK: bind-key-pkg.el in the emacs-straight mirror tries to set the mode to
  ;;   lisp-data-mode, which doesn't exist prior to Emacs 28.x, so bind-key will
  ;;   fail to build for those users. Until we drop 27.x support, we omit it.
  :recipe (:files ("bind-key.el"))
  :pin "ec9d0505febe2556b47457355763f5f1408a35ac")

(package! use-package :pin "c95bceeadad393ef5e9c85e3172e3434f9fbaaca")
