;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "7523823ca3709e0327f3e9f38ddfec71a58084be")
  (package! undo-fu :pin "c0806c1903c5a0e4c69b6615cdc3366470a9b8ca")
  (package! undo-fu-session :pin "5135b5cb610203cdb3e64227b8f437bb87292b18"))
