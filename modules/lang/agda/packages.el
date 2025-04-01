;; -*- no-byte-compile: t; -*-
;;; lang/agda/packages.el

(unless (modulep! +local)
  (package! agda-input
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/agda-input.el")
             :nonrecursive t)
    :pin "10a0ca05161d1d5c349e0ae3b5f883e29526a65e")

  (package! agda2-mode
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/*.el"
                     (:exclude "agda-input.el"))
             :nonrecursive t)
    :pin "10a0ca05161d1d5c349e0ae3b5f883e29526a65e"))
