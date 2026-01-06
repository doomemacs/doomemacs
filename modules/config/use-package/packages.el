;; -*- no-byte-compile: t; -*-
;;; core/use-package/packages.el

(package! bind-key
  ;; HACK: bind-key-pkg.el in the emacs-straight mirror tries to set the mode to
  ;;   lisp-data-mode, which doesn't exist prior to Emacs 28.x, so bind-key will
  ;;   fail to build for those users. Until we drop 27.x support, we omit it.
  :recipe (:files ("bind-key.el"))
  :pin "e9ead409b17d8cf7c221f6c28e115a8c051be9e3")

(package! use-package :pin "039b721a4d24b93adc3170e39a6cba7a8200ed43")
