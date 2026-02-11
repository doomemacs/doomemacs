;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "d8f72bbe7d3c3a2808986febd3bb1a46d4da7f51")
  (package! undo-fu
    :recipe (:host github :repo "emacsmirror/undo-fu")
    :pin "b4ce5ed20c1cf0591e497e6998a7634a172726fa")
  (package! undo-fu-session
    :recipe (:host github :repo "emacsmirror/undo-fu-session")
    :pin "34ae31308d2f290f87a330c76a627b7fe8ebb720")
  (when (> emacs-major-version 27)  ; unsupported in 27
    (package! vundo :pin "288d1b06ea9283852640bb49532e57ae0515492f")))
