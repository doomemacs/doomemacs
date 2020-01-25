;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "f830f209dd")
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil :pin "f830f209dd"))
(package! treemacs-projectile :pin "f830f209dd")
(when (featurep! :tools magit)
  (package! treemacs-magit :pin "f830f209dd"))
(when (featurep! :ui workspaces)
  (package! treemacs-persp :pin "f830f209dd"))
