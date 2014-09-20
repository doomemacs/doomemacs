(provide 'init-git)

(use-package git-commit-mode
  :mode (("/COMMIT_EDITMSG\\'" . git-commit-mode)
         ("/NOTES_EDITMSG\\'" . git-commit-mode)
         ("/MERGE_MSG\\'" . git-commit-mode)
         ("/TAG_EDITMSG\\'" . git-commit-mode)
         ("/PULLREQ_EDITMSG\\'" . git-commit-mode)))

(use-package git-rebase-mode
  :mode ("/git-rebase-todo\\'" . git-rebase-mode))

(use-package gitconfig-mode
  :mode (("/\\.?git/?config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode))
  :config
  (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :init
  (global-git-gutter+-mode 1))
