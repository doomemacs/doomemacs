(require-packages
 '(git-commit-mode
   git-rebase-mode
   gitconfig-mode
   gitignore-mode
   git-gutter-fringe
   ))

(global-git-gutter-mode t)
(setq git-gutter-fr:side 'right-fringe)
;; (diminish 'git-gutter-mode)

;;
(provide 'mod-git)
