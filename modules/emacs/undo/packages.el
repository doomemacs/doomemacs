;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "d8f72bbe7d3c3a2808986febd3bb1a46d4da7f51")
  (package! undo-fu
    :recipe (:host github :repo "emacsmirror/undo-fu")
    :pin "17fc25be69c06225b5b74f65e7ff6c212919bd3f")
  (package! undo-fu-session
    :recipe (:host github :repo "emacsmirror/undo-fu-session")
    :pin "58bd7f321e6fb2a359d32f23a962fd1d74966314")
  (when (> emacs-major-version 27)  ; unsupported in 27
    (package! vundo :pin "288d1b06ea9283852640bb49532e57ae0515492f")))
