;; -*- no-byte-compile: t; -*-
;;; lang/agda/packages.el

(unless (featurep! +local)
  (package! agda-input
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/agda-input.el")
             :nonrecursive t)
    :pin "8eb0d01811a663cf2b27b482b3b18690adfa094b")

  (package! agda2-mode
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/*.el"
                     (:exclude "agda-input.el"))
             :nonrecursive t)
    :pin "8eb0d01811a663cf2b27b482b3b18690adfa094b"))
