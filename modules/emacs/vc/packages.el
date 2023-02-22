;; -*- no-byte-compile: t; -*-
;;; emacs/vc/packages.el

(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)

(package! browse-at-remote :pin "c020975a891438e278ad1855213d4f3d62c9fccb")
(package! git-commit :pin "97a95f70079b6613bf98d2306279d3e03fe51234")
(package! git-timemachine
  ;; The original lives on codeberg.org; which has uptime issues.
  :recipe (:host github :repo "emacsmirror/git-timemachine")
  :pin "d8ffd0d7cc4ab3dd7de494c9ea36dfd99e2744fa")
(package! git-modes :pin "f0a0154bf48dd1c0c587596cf4cfd3c90f673a05")
