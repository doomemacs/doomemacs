;;; editor/meow/config.el -*- lexical-binding: t; -*-

(use-package! meow
  :hook (doom-init-modules . meow-global-mode)
  :config
  (cond
   ((featurep! :editor meow +colemak) (meow--setup-colemak))
   ((featurep! :editor meow +dvorak) (meow--setup-dvorak))
   ((featurep! :editor meow +dvp) (meow--setup-dvp))
   ((featurep! :editor meow +qwerty) (meow--setup-qwerty))
   ((featurep! :editor meow +useful-keybindings) (meow--setup-useful-keybindings))))
