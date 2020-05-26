;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "5b6df03781495d8a25695d846b0cce496d3d3058")
  (package! undo-fu :pin "46de023b5f8ddb989eeff7665feeec2877d8eda8")
  (package! undo-fu-session :pin "e2043f8350970e1a9ef06a94956a733826cdf32b"))
