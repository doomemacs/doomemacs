;; -*- no-byte-compile: t; -*-
;;; tools/docker/packages.el

(package! docker :pin "cc0046e6a557dce0ccc4108dd22e04f21ba8b0dc")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  (package! docker-tramp :pin "930d7b46c180d8a13240a028c1b40af84f2a3219"))
(package! dockerfile-mode :pin "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c")
