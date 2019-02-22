;; -*- no-byte-compile: t; -*-
;;; tools/vterm/packages.el

(package! vterm :recipe
  (:fetcher github
   :repo "akermu/emacs-libvterm"
   :files ("*")))
