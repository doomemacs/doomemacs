;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! dirvish
  :recipe (:host github :repo "hlissner/dirvish")
  :pin "5f046190e886fb0a2dae7e884cc7cd9bcf48ac26")
(package! diredfl :pin "fe72d2e42ee18bf6228bba9d7086de4098f18a70")
