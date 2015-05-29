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
  :config
  (progn
    (setq diff-hl-draw-borders nil)
    (global-diff-hl-mode +1)))

;; (use-package git-gutter-fringe+
;;   :diminish (git-gutter+-mode . " git")
;;   :config
;;   (progn
;;     ;; (global-git-gutter+-mode +1)
;;     (add-hooks '(text-mode-hook prog-mode-hook) 'git-gutter+-mode)
;;     ;; Fixes "git-gutter+-process-diff: Wrong number of arguments: nil" error
;;     (defadvice git-gutter+-process-diff (before git-gutter+-process-diff-advice activate)
;;       (ad-set-arg 0 (file-truename (ad-get-arg 0))))

;;     (fringe-helper-define 'git-gutter-fr+-added nil
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X....")

;;     (fringe-helper-define 'git-gutter-fr+-deleted nil
;;       "....X...."
;;       "....XXXXX"
;;       "....XXXXX"
;;       "....X...."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       "........."
;;       ".........")

;;     (fringe-helper-define 'git-gutter-fr+-modified nil
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X...."
;;       "....X....")))


(provide 'init-vc)
;;; init-vc.el ends here
