;; -*- no-byte-compile: t; -*-
;;; ui/unicode/packages.el

(package! unicode-fonts :pin "47f2397ade28eba621baa865fd69e4efb71407a5")
(package! pcache
  ;; Use a fork of `pcache', which has been patched to support Emacs 29 (the
  ;; original has been abandoned).
  :recipe (:host github :repo "LemonBreezes/pcache")
  :pin "2004af4cf978ca8fcbeaa4da6b9b12600a202aeb")
