;; -*- no-byte-compile: t; -*-
;;; emacs/vc/packages.el

(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)

(package! browse-at-remote :pin "fadf99d6d8e891f3b112e36c772e0eea0b9bc7f2")
(package! git-commit :pin "577f16da3072a0ae85fba6b25a36a971a61ec6c2")
(package! git-timemachine :pin "8d675750e921a047707fcdc36d84f8439b19a907")
(package! gitconfig-mode :pin "14adca24eb6b0b4e311ad144c5d41972c6b044b2")
(package! gitignore-mode :pin "14adca24eb6b0b4e311ad144c5d41972c6b044b2")
