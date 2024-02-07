;; -*- no-byte-compile: t; -*-
;;; emacs/vc/packages.el

(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)

(package! browse-at-remote :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf")
(package! git-commit :pin "b68e0a3c3388af8daac662f25ccfd3e980590e12")
(package! git-timemachine
  ;; The original lives on codeberg.org; which has uptime issues.
  :recipe (:host github :repo "emacsmirror/git-timemachine")
  :pin "ac933e5cd29583c131401f3bd991d98129c316df")
(package! git-modes :pin "3cc94974c09c43462dfbfbe20396a414352dbb92")
