;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "d8f72bbe7d3c3a2808986febd3bb1a46d4da7f51")
  (package! undo-fu
    :recipe (:host github :repo "emacsmirror/undo-fu")
    :pin "5684ef2aef5f60176472916b21869cf221e018cc")
  (package! undo-fu-session
    :recipe (:host github :repo "emacsmirror/undo-fu-session")
    :pin "92d733a5b162a70c572fac17b9f9e872426df547")
  (when (> emacs-major-version 27)  ; unsupported in 27
    (package! vundo :pin "288d1b06ea9283852640bb49532e57ae0515492f")))
