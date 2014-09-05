(provide 'init-project)

(add-hook 'dired-load-hook
          (lambda()
            (use-package dired+ :config
              (setq dired-recursive-deletes 'always
                    dired-recursive-copies 'always

                    ;; if there is a dired buffer displayed in the next window, use its
                    ;; current subdir, instead of the current subdir of this dired buffer
                    dired-dwim-target t))))

(use-package helm :defer t)
(use-package grizzl :defer t)

(use-package neotree
  :commands
  (neotree-show neotree-hide neotree-toggle))

(use-package projectile
  :diminish projectile-mode
  :config
  (progn (projectile-global-mode)
         (setq projectile-completion-system 'grizzl
               projectile-enable-caching t)))

(use-package ag
  :commands (ag ag-search ag-regexp)
  :config
  (progn
    (setq ag-reuse-window t)
    (setq ag-reuse-buffers t)
    (setq ag-highlight-search t)
    (define-key ag-mode-map [escape] 'ag-kill-buffers)
    (define-key ag-mode-map "h" nil)))
