;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "7523823ca3709e0327f3e9f38ddfec71a58084be")
  (package! undo-fu :pin "c0806c1903c5a0e4c69b6615cdc3366470a9b8ca")
  (package! undo-fu-session :pin "e2043f8350970e1a9ef06a94956a733826cdf32b"))
