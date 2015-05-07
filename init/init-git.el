(use-package git-commit-mode            ;
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
  :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode (("/\\.gitignore\\'"         . gitignore-mode)
         ("/\\.git/info/exclude\\'"  . gitignore-mode)
         ("/git/ignore\\'"           . gitignore-mode)))
;;
(use-package git-gutter-fringe+
  :config
  (progn
    (global-git-gutter+-mode +1)

    ;; Fixes "git-gutter+-process-diff: Wrong number of arguments: nil" error
    (defadvice git-gutter+-process-diff (before git-gutter+-process-diff-advice activate)
      (ad-set-arg 0 (file-truename (ad-get-arg 0))))

    (fringe-helper-define 'git-gutter-fr+-added nil
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......")

    (fringe-helper-define 'git-gutter-fr+-deleted nil
      "XX......"
      "XXXXXXXX"
      "XX......"
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
      "........"
      "........")

    (fringe-helper-define 'git-gutter-fr+-modified nil
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......")))

(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'git-rebase-mode 'insert)


(provide 'init-git)
;;; init-git.el ends here
