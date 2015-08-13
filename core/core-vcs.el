;;; core-vcs.el --- version control awareness

(use-package git-commit-mode            ;
  :mode ("/COMMIT_EDITMSG$"
         "/NOTES_EDITMSG$"
         "/MERGE_MSG$"
         "/TAG_EDITMSG$"
         "/PULLREQ_EDITMSG$")
  :config
  (evil-set-initial-state 'git-commit-mode 'insert))

(use-package git-rebase-mode
  :mode "/git-rebase-todo$"
  :config
  (evil-set-initial-state 'git-rebase-mode 'insert))

(use-package gitconfig-mode
  :mode ("/\\.?git/?config$" "/\\.gitmodules$")
  :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode ("/\\.gitignore$"
         "/\\.git/info/exclude$"
         "/git/ignore$"))

(use-package diff-hl
  :init (setq diff-hl-draw-borders nil)
  :config (global-diff-hl-mode +1))

(provide 'core-vcs)
;;; core-vcs.el ends here
