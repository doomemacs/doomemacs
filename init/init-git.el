(provide 'init-git)

(use-package git-commit-mode
  :mode (("/COMMIT_EDITMSG\\'"       . git-commit-mode)
         ("/NOTES_EDITMSG\\'"        . git-commit-mode)
         ("/MERGE_MSG\\'"            . git-commit-mode)
         ("/TAG_EDITMSG\\'"          . git-commit-mode)
         ("/PULLREQ_EDITMSG\\'"      . git-commit-mode)))

(use-package git-rebase-mode
  :mode ("/git-rebase-todo\\'"       . git-rebase-mode))

(use-package gitconfig-mode
  :mode (("/\\.?git/?config\\'"      . gitconfig-mode)
         ("/\\.gitmodules\\'"        . gitconfig-mode))
  :config (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode (("/\\.gitignore\\'"         . gitignore-mode)
         ("/\\.git/info/exclude\\'"  . gitignore-mode)
         ("/git/ignore\\'"           . gitignore-mode)))

(use-package git-gutter-fringe+
  :init (global-git-gutter+-mode +1)
  :config
  (progn
    (fringe-helper-define 'git-gutter-fr+-added nil
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X.......")

    (fringe-helper-define 'git-gutter-fr+-deleted nil
      "X......."
      "XXXXXXXX"
      "X......."
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........")

    (fringe-helper-define 'git-gutter-fr+-modified nil
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X......."
      "X.......")))
