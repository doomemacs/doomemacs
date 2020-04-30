;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (featurep! +tree)
    (package! undo-tree :pin "5b6df03781495d8a25695d846b0cce496d3d3058")
  (package! undo-fu :pin "0ce9ac36144e80316fff50bfe1bc5dd7e5e7ded6")
  (package! undo-fu-session :pin "b808ef0cdcdd2eef221c67eda567eed7fcb3d4af"))
