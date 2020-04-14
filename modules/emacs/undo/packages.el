;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "5b6df03781")
  (package! undo-fu :pin "0c34b6747e")
  (package! undo-fu-session :pin "b808ef0cdc"))
