;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "d8f72bbe7d3c3a2808986febd3bb1a46d4da7f51")
  (package! undo-fu
    :recipe (:host github :repo "emacsmirror/undo-fu")
    :pin "399cc12f907f81a709f9014b6fad0205700d5772")
  (package! undo-fu-session
    :recipe (:host github :repo "emacsmirror/undo-fu-session")
    :pin "d90d42ddba8fa42ef5dc109196545caeabb42b75")
  (when (> emacs-major-version 27)  ; unsupported in 27
    (package! vundo :pin "5a666b34e4f609cca77b4a07b97b38581deb7e7e")))
