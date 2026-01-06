;; -*- no-byte-compile: t; -*-
;;; tools/docker/packages.el

(package! docker :pin "375e0ed45bb1edc655d9ae2943a09864bec1fcba")
;; tramp-container (included with Emacs 29+) replaces docker-tramp
(when (< emacs-major-version 29)
  (package! docker-tramp :pin "19d0771db4e6b89e19c00af5806438e315779c15"))
(package! dockerfile-mode :pin "97733ce074b1252c1270fd5e8a53d178b66668ed")
