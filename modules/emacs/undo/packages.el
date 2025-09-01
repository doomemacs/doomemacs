;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "d8f72bbe7d3c3a2808986febd3bb1a46d4da7f51")
  (package! undo-fu
    :recipe (:host github :repo "emacsmirror/undo-fu")
    :pin "545e29459e71a9aca81c96c1385d43a5696e27e9")
  (package! undo-fu-session
    :recipe (:host github :repo "emacsmirror/undo-fu-session")
    :pin "99d1b5099f432123577c51b6a67210b0c37716e6")
  (when (> emacs-major-version 27)  ; unsupported in 27
    (package! vundo :pin "f57937d7f57e6d081f567debf14f11d87a28962f")))
