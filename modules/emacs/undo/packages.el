;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "e326c6135e62f5fe8536528d3acd5e798f847407")
  (package! undo-fu :pin "e0ad06b5ef2ac2733dad2ad48e3957b5c36edfa5")
  (package! undo-fu-session :pin "243d93b4c7c1224e7067cd323f64d23dfdfe7c0e"))
