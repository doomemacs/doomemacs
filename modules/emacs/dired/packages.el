;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! dirvish
  :recipe (:host github :repo "hlissner/dirvish")
  :pin "5f046190e886fb0a2dae7e884cc7cd9bcf48ac26")
(package! diredfl :pin "f6d599c30875ab4894c1deab9713ff2faea54e06")
