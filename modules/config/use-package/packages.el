;; -*- no-byte-compile: t; -*-
;;; core/use-package/packages.el

(package! bind-key
  ;; HACK: bind-key-pkg.el in the emacs-straight mirror tries to set the mode to
  ;;   lisp-data-mode, which doesn't exist prior to Emacs 28.x, so bind-key will
  ;;   fail to build for those users. Until we drop 27.x support, we omit it.
  :recipe (:files ("bind-key.el"))
  :pin "aa22c8c3c740c2f306509b9c37d9511cfa41b612")

(package! use-package :pin "bbfe01bdf15eeb61babffd1c5b6facd3d2ce3630")
