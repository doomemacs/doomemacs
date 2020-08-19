;; -*- no-byte-compile: t; -*-
;;; lang/agda/packages.el

(unless (featurep! +local)
  (package! agda-input
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/agda-input.el")
             :nonrecursive t)
    :pin "36738c13e2e85f1029083e974cdcff6927d48980")

  (package! agda2-mode
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/*.el"
                     (:exclude "agda-input.el"))
             :nonrecursive t)
    :pin "36738c13e2e85f1029083e974cdcff6927d48980"))
