;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "5e9f4c92348b3cfd6f140c1730294017dca4bc05")
  (package! undo-fu :pin "ea902716f39c725db1b90dbb285b44404b3bd6df")
  (package! undo-fu-session :pin "75d78310c86901f01b35d47f3ac1a5c8368abba4")
  (when (> emacs-major-version 27)  ; unsupported in 27
    (package! vundo :pin "a18d63a9a15d363c11e2405934e709723e472d86")))
