;; -*- no-byte-compile: t; -*-
;;; emacs/vc/packages.el

(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)

(package! browse-at-remote :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf")
(package! git-commit :pin "f9268a959828d0c6ab26171dd2fb1ffc55e5ae70")
(package! git-timemachine
  ;; The original lives on codeberg.org; which has uptime issues.
  :recipe (:host github :repo "emacsmirror/git-timemachine")
  :pin "5ed73c3831cf6da10ba941e6abba708a86853e8f")
(package! git-modes :pin "52ea2a1281ea9df9b8732fe2add0e6a0c9c2cd11")
