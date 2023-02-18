;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "16f4121032d09ef44b3d7d02c4d02c3c2f18041f")
  (package! undo-fu :pin "0e22308de8337a9291ddd589edae167d458fbe77")
  (package! undo-fu-session :pin "7b3fd0647dd1fbd02101eec61440e6d44953bcd9"))
