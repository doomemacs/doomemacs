;; -*- no-byte-compile: t; -*-
;;; ui/minimap/packages.el

(package! minimap
  :recipe (:host github
           :repo "emacs-straight/minimap"
           :files ("minimap.el"))
  :pin "d8850be6fb7b3dbff0cd9b5a04d10a6dcc527326")
