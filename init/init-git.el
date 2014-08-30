(use-package git-commit-mode
  :mode (("/COMMIT_EDITMSG\\'" . git-commit-mode)
         ("/NOTES_EDITMSG\\'" . git-commit-mode)
         ("/MERGE_MSG\\'" . git-commit-mode)
         ("/TAG_EDITMSG\\'" . git-commit-mode)
         ("/PULLREQ_EDITMSG\\'" . git-commit-mode)))

(use-package git-rebase-mode
  :mode ("/git-rebase-todo\\'" . git-rebase-mode))

(use-package gitconfig-mode
  :mode (("/\\.gitconfig\\'" . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/git/config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode))
  :config (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :init
  (progn
    (global-git-gutter-mode t)
    (add-hook 'git-gutter-mode-on-hook
              (lambda() (fringe-mode '(4 . 8)))))
  :config
  (progn
    (custom-set-variables '(git-gutter:lighter " !"))
    (custom-set-variables '(git-gutter:verbosity 0))

    (set-face-foreground 'git-gutter-fr:modified "#444444")
    (set-face-background 'git-gutter-fr:modified "#444444")
    (set-face-foreground 'git-gutter-fr:deleted "#884444")
    (set-face-background 'git-gutter-fr:deleted "#884444")
    (set-face-foreground 'git-gutter-fr:added "#448844")
    (set-face-background 'git-gutter-fr:added "#448844")
    ))

;;
(provide 'init-git)
