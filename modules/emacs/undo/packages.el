;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "e326c6135e62f5fe8536528d3acd5e798f847407")
  (package! undo-fu :pin "34b27c01da4c3eb8aa595f3613b7e2e1ed4e54be")
  (package! undo-fu-session :pin "579936966b41d2d6782f587509fef21477141374"))
