;; -*- no-byte-compile: t; -*-
;;; lang/agda/packages.el

(unless (featurep! +local)
  (package! agda-input
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/agda-input.el")
             :nonrecursive t)
    :pin "b612cabaae94e8b261491c3bc718ffb4805236f0")

  (package! agda2-mode
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/*.el"
                     (:exclude "agda-input.el"))
             :nonrecursive t)
    :pin "b612cabaae94e8b261491c3bc718ffb4805236f0"))
