;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "4eb8eb8821")
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil :pin "4eb8eb8821"))
(package! treemacs-projectile :pin "4eb8eb8821")
(when (featurep! :tools magit)
  (package! treemacs-magit :pin "4eb8eb8821"))
