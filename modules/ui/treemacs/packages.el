;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "d539f7bfa0cf97383a3e15688d904c14d4d94aa9")
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil :pin "d539f7bfa0cf97383a3e15688d904c14d4d94aa9"))
(package! treemacs-projectile)
(when (featurep! :tools magit)
  (package! treemacs-magit :pin "d539f7bfa0cf97383a3e15688d904c14d4d94aa9"))
(when (featurep! :ui workspaces)
  (package! treemacs-persp :pin "d539f7bfa0cf97383a3e15688d904c14d4d94aa9"))
