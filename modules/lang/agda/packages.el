;; -*- no-byte-compile: t; -*-
;;; lang/agda/packages.el

(unless (modulep! +local)
  (package! agda-input
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/agda-input.el")
             :nonrecursive t)
    :pin "4f82f9b90afbcc72e528ebde08ff6b9bef5b6a9a")

  (package! agda2-mode
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/*.el"
                     (:exclude "agda-input.el"))
             :nonrecursive t)
    :pin "4f82f9b90afbcc72e528ebde08ff6b9bef5b6a9a"))
