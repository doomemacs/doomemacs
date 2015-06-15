;;; core-vcs.el --- version control awareness

(use-package git-commit-mode            ;
  :mode (("/COMMIT_EDITMSG\\'"       . git-commit-mode)
         ("/NOTES_EDITMSG\\'"        . git-commit-mode)
         ("/MERGE_MSG\\'"            . git-commit-mode)
         ("/TAG_EDITMSG\\'"          . git-commit-mode)
         ("/PULLREQ_EDITMSG\\'"      . git-commit-mode))
  :config
  (evil-set-initial-state 'git-commit-mode 'insert))

(use-package git-rebase-mode
  :mode ("/git-rebase-todo\\'"       . git-rebase-mode)
  :config
  (evil-set-initial-state 'git-rebase-mode 'insert))

(use-package gitconfig-mode
  :mode (("/\\.?git/?config\\'"      . gitconfig-mode)
         ("/\\.gitmodules\\'"        . gitconfig-mode))
  :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode (("/\\.gitignore\\'"         . gitignore-mode)
         ("/\\.git/info/exclude\\'"  . gitignore-mode)
         ("/git/ignore\\'"           . gitignore-mode)))

(use-package diff-hl
  :init (setq diff-hl-draw-borders nil)
  :config (global-diff-hl-mode +1))

(provide 'core-vcs)
;;; core-vcs.el ends here
