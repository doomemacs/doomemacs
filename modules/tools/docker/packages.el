;; -*- no-byte-compile: t; -*-
;;; tools/docker/packages.el

(package! docker :pin "3bea08f1d221ef9aefd9c05954423175a42d8a7e")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
(package! dockerfile-mode :pin "8135740bfc6ad96ab82d39d9fe68dbce56180f4c")
