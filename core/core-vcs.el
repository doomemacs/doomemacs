;;; core-vcs.el --- version control awareness

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
