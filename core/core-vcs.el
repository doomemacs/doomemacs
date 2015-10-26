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

(use-package github-browse-file
  :commands (narf:github-browse-file github-browse-file github-browse-file-blame)
  :config
  (evil-define-command narf:github-browse-file (&optional bang)
    (interactive "<!>")
    (if bang (github-browse-file-blame) (github-browse-file))))

(provide 'core-vcs)
;;; core-vcs.el ends here
