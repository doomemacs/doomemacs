;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "05d264ae8c")
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil :pin "05d264ae8c"))
(package! treemacs-projectile :pin "05d264ae8c")
(when (featurep! :tools magit)
  (package! treemacs-magit :pin "05d264ae8c"))
(when (featurep! :ui workspaces)
  (package! treemacs-persp :pin "05d264ae8c"))
