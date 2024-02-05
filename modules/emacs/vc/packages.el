;; -*- no-byte-compile: t; -*-
;;; emacs/vc/packages.el

(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)

(package! browse-at-remote :pin "76aa27dfd469fcae75ed7031bb73830831aaccbf")
(package! git-commit :pin "54d37dc14c3f715dd0328a70bc65d63c54ee9613")
(package! git-timemachine
  ;; The original lives on codeberg.org; which has uptime issues.
  :recipe (:host github :repo "emacsmirror/git-timemachine")
  :pin "ac933e5cd29583c131401f3bd991d98129c316df")
(package! git-modes :pin "4a61a9b86df9c824a99c522f42d55e68faf85f91")
