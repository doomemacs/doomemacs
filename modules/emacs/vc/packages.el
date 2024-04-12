;; -*- no-byte-compile: t; -*-
;;; emacs/vc/packages.el

(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)

(package! browse-at-remote :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf")
(package! git-commit :pin "b5637d665c1e5bd5b76ffb072dbac387f37a5f63")
(package! git-timemachine
  ;; The original lives on codeberg.org; which has uptime issues.
  :recipe (:host github :repo "emacsmirror/git-timemachine")
  :pin "5ed73c3831cf6da10ba941e6abba708a86853e8f")
(package! git-modes :pin "3cc94974c09c43462dfbfbe20396a414352dbb92")
