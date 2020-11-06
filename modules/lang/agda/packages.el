;; -*- no-byte-compile: t; -*-
;;; lang/agda/packages.el

(unless (featurep! +local)
  (package! agda-input
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/agda-input.el")
             :nonrecursive t)
    :pin "08191e66e65a4870bf43e21007909fc03150eabf")

  (package! agda2-mode
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/*.el"
                     (:exclude "agda-input.el"))
             :nonrecursive t)
    :pin "08191e66e65a4870bf43e21007909fc03150eabf"))
