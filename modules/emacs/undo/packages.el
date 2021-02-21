;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "e326c6135e62f5fe8536528d3acd5e798f847407")
  (package! undo-fu :pin "f4db4c9b9875134df6f5279281099361ae11c2e9")
  (package! undo-fu-session :pin "a0389147365c10c974ad68b797b185affb935fe3"))
