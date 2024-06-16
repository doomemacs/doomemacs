;; -*- no-byte-compile: t; -*-
;;; lang/agda/packages.el

(unless (modulep! +local)
  (package! agda-input
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/agda-input.el")
             :nonrecursive t)
    :pin "fbf9d159c3c874b8328ccdc78a0d57d57a310234")

  (package! agda2-mode
    :recipe (:host github :repo "agda/agda"
             :files ("src/data/emacs-mode/*.el"
                     (:exclude "agda-input.el"))
             :nonrecursive t)
    :pin "fbf9d159c3c874b8328ccdc78a0d57d57a310234"))
