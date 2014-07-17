(require-packages
 '(git-commit-mode
   git-rebase-mode
   gitconfig-mode
   gitignore-mode
   git-gutter-fringe
   ))

(custom-set-variables '(git-gutter:lighter " !"))
(custom-set-variables '(git-gutter:verbosity 0))

(set-face-foreground 'git-gutter-fr:modified "#555555")
(set-face-background 'git-gutter-fr:modified "#444444")
(set-face-foreground 'git-gutter-fr:deleted "#995555")
(set-face-background 'git-gutter-fr:deleted "#884444")
(set-face-foreground 'git-gutter-fr:added "#559955")
(set-face-background 'git-gutter-fr:added "#448844")

; (setq git-gutter-fr:side 'right-fringe)
(global-git-gutter-mode t)

(add-hook 'git-gutter-mode-on-hook
          (lambda() (fringe-mode '(4 . 8))))
;;
(provide 'mod-git)
