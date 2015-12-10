;;; core-vcs.el --- version control awareness

(use-package gitconfig-mode
  :mode ("/\\.?git/?config$" "/\\.gitmodules$")
  :init (add-hook! gitconfig-mode 'flyspell-mode))

(use-package gitignore-mode
  :mode ("/\\.gitignore$"
         "/\\.git/info/exclude$"
         "/git/ignore$"))

(use-package github-browse-file
  :commands (github-browse-file github-browse-file-blame)
  :init
  (evil-define-command narf:github-browse-file (&optional bang)
    (interactive "<!>")
    (if bang (github-browse-file-blame) (github-browse-file))))

(use-package git-gutter
  :commands (git-gutter-mode narf/vcs-next-hunk narf/vcs-prev-hunk
             narf/vcs-show-hunk narf/vcs-stage-hunk narf/vcs-revert-hunk)
  :init
  (add-hook! (text-mode prog-mode) 'git-gutter-mode)
  :config
  (require 'git-gutter-fringe)

  (setq git-gutter:update-interval 2)

  (defalias 'narf/vcs-next-hunk    'git-gutter:next-hunk)
  (defalias 'narf/vcs-prev-hunk    'git-gutter:previous-hunk)
  (defalias 'narf/vcs-show-hunk    'git-gutter:popup-hunk)
  (defalias 'narf/vcs-stage-hunk   'git-gutter:stage-hunk)
  (defalias 'narf/vcs-revert-hunk  'git-gutter:revert-hunk)

  (define-fringe-bitmap 'git-gutter-fr:added
    [240 240 240 240 240 240 240 240 240 240 240 240 240 240]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [240 240 240 240 240 240 240 240 240 240 240 240 240 240]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 128 192 224 240 248]
    nil nil 'center)

  (advice-add 'evil-force-normal-state :after 'git-gutter)

  (add-hook! focus-in 'git-gutter:update-all-windows)
  (add-hook! evil-insert-state-exit 'git-gutter))

(use-package diff-hl
  :disabled t
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-fringe-bmp-function 'narf-diff-hl-fringe-bmp)
  :config
  (defalias narf/vcs-next-hunk 'diff-hl-next-hunk)
  (defalias narf/vcs-prev-hunk 'diff-hl-previous-hunk)
  ;; (defalias narf/vcs-show-hunk
  ;; (defalias narf/vcs-stage-hunk ...)
  (defalias narf/vcs-revert-hunk 'diff-hl-revert-hunk)

  (defun narf-diff-hl-fringe-bmp (type _pos)
    (if (eq type 'delete)
        'narf--diff-hl-bitmap-del
      'narf--diff-hl-bitmap))

  (map! :map diff-mode-shared-map
        :n "RET" 'diff-goto-source)

  (define-fringe-bitmap 'narf--diff-hl-bitmap
    [240 240 240 240 240 240 240 240 240 240 240 240 240 240]
    nil nil 'center)
  (define-fringe-bitmap 'narf--diff-hl-bitmap-del
    [248 240 224 192 128 0 0 0 0 0 0 0 0 0]
    nil nil 'center)

  (global-diff-hl-mode 1))

(provide 'core-vcs)
;;; core-vcs.el ends here
