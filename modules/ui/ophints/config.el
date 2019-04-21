;;; ui/ophints/config.el -*- lexical-binding: t; -*-

(def-package! evil-goggles
  :when (featurep! :editor evil)
  :after-call pre-command-hook
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-pulse nil ; too slow
        ;; evil-goggles provides a good indicator of what has been affected.
        ;; delete/change is obvious, so I'd rather disable it for these.
        evil-goggles-enable-delete nil
        evil-goggles-enable-change nil)
  :config
  (evil-goggles-mode +1))


(def-package! volatile-highlights
  :unless (featurep! :editor evil)
  :after-call pre-command-hook
  :config
  (volatile-highlights-mode)
  (after! undo-tree
    (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
    (vhl/install-extension 'undo-tree)))
