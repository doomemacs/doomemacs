;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "e326c6135e62f5fe8536528d3acd5e798f847407")
  (package! undo-fu :pin "ab8bc10e424bccc847800c31ab41888db789d55d")
  (package! undo-fu-session :pin "1810251485a551bc41472ec9e7e7bfab72a45a3c"))
