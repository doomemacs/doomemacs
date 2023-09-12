;; -*- no-byte-compile: t; -*-
;;; tools/docker/packages.el

(package! docker :pin "6997c86a24d440fa04035aa8c9499a52df6e655d")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
(package! dockerfile-mode :pin "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c")
