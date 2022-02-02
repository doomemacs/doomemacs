;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "e326c6135e62f5fe8536528d3acd5e798f847407")
  (package! undo-fu :pin "ab8bc10e424bccc847800c31ab41888db789d55d")
  (package! undo-fu-session :pin "edf050d6133478d04fc06cc65914517b18d6bcc6"))
