;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "5b6df03781495d8a25695d846b0cce496d3d3058")
  (package! undo-fu :pin "2b1e53285a55ce50ca6fd60b050f2171e235d8f9")
  (package! undo-fu-session :pin "0400f15f2a0cfcedb69c06c3ff62f3f8814b62fb"))
