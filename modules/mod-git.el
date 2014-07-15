(require-packages
 '(magit
   gitconfig-mode
   gitignore-mode
   git-gutter
   ))

(git-gutter:linum-setup)
(global-git-gutter-mode +1)

;;
(provide 'mod-git)
