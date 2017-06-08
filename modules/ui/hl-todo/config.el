;;; ui/hl-todo/packages.el -*- lexical-binding: t; -*-

(def-package! hl-todo
  :commands hl-todo-mode
  :init (add-hook 'prog-mode-hook #'hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))
