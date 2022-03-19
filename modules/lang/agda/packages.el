;; -*- no-byte-compile: t; -*-
;;; lang/agda/packages.el

(unless (featurep! +local)
  (package! agda-input
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/agda-input.el")
             :nonrecursive t)
    :pin "2816605bfdc24c93d8801016d31beeed7608ad02")

  (package! agda2-mode
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/*.el"
                     (:exclude "agda-input.el"))
             :nonrecursive t)
    :pin "2816605bfdc24c93d8801016d31beeed7608ad02"))
