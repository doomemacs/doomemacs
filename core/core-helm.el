;;; core-helm.el

(use-package helm
  :defer 1
  :commands (helm helm-other-buffer helm-mode)
  :init
  (defvar helm-global-prompt "››› ")
  (setq-default
   helm-quick-update t
   ;; Speedier without fuzzy matching
   helm-mode-fuzzy-match nil
   helm-buffers-fuzzy-matching nil
   helm-apropos-fuzzy-match nil
   helm-M-x-fuzzy-match nil
   helm-recentf-fuzzy-match nil
   helm-projectile-fuzzy-match nil
   ;; Display extraineous helm UI elements
   helm-display-header-line nil
   helm-ff-auto-update-initial-value nil
   helm-find-files-doc-header nil
   ;; Don't override evil-ex's completion
   helm-mode-handle-completion-in-region nil
   helm-candidate-number-limit 50
   ;; Don't wrap item cycling
   helm-move-to-line-cycle-in-source t)

  (defvar helm-projectile-find-file-map (make-sparse-keymap))

  :config
  (set-keymap-parent helm-projectile-find-file-map helm-map)
  (map! :map helm-map
        "C-S-n"      'helm-next-source
        "C-S-p"      'helm-previous-source
        "C-u"        'helm-delete-minibuffer-contents
        "C-w"        'backward-kill-word
        "M-v"        'clipboard-yank
        "C-r"        'evil-paste-from-register ; Evil registers in helm! Glorious!
        "C-b"        'backward-word
        "<left>"     'backward-char
        "<right>"    'forward-char
        "<escape>"   'helm-keyboard-quit
        "ESC"        'helm-keyboard-quit
        [escape]     'helm-keyboard-quit
        "<tab>"      'helm-execute-persistent-action
        :map helm-generic-files-map
        :e "ESC"     'helm-keyboard-quit)

  ;;; Helm hacks
  (defvar doom-helm-header-fg (face-attribute 'helm-source-header :foreground) "docstring")

  ;; Shrink source headers if there is only one source
  (add-hook 'helm-after-initialize-hook 'doom*helm-hide-source-header-maybe)
  ;; A simpler prompt: see `helm-global-prompt'
  (advice-add 'helm :filter-args 'doom*helm-replace-prompt)
  ;; Hide mode-line in helm windows
  (advice-add 'helm-display-mode-line :override 'doom*helm-hide-header))

(use-package helm-mode
  :after helm
  :config (helm-mode 1))

(use-package helm-locate
  :defer t
  :init
  (defvar helm-generic-files-map (make-sparse-keymap)
    "Generic Keymap for files.")
  :config (set-keymap-parent helm-generic-files-map helm-map))

(use-package helm-buffers
  :commands (helm-buffers-list helm-mini)
  :config
  (defvar doom-helm-force-project-buffers nil)
  (defun helm*buffer-list (&rest _) (doom/get-buffer-names doom-helm-force-project-buffers))
  (advice-add 'helm-buffer-list :override 'helm*buffer-list))

(use-package helm-dash
  :when (not IS-WINDOWS)
  :commands (helm-dash helm-dash-at-point helm-dash-install-docset def-docset!)
  :config
  (setq helm-dash-browser-func 'doom/dash-open
        helm-dash-candidate-format "%d → %n (%t)")
  (defmacro def-docset! (mode docsets)
    `(add-hook! ,mode (setq-local helm-dash-docsets ',docsets))))

(use-package helm-tags
  :commands (helm-tags-get-tag-file helm-etags-select))

(use-package helm-bookmark
  :commands (helm-bookmarks helm-filtered-bookmarks)
  :config (setq-default helm-bookmark-show-location t))

(use-package helm-projectile
  :commands (helm-projectile-find-other-file
             helm-projectile-switch-project
             helm-projectile-find-file
             helm-projectile-find-dir))

(use-package helm-files
  :commands (helm-browse-project helm-find helm-find-files helm-for-files helm-multi-files helm-recentf)
  :config
  (map! :map helm-find-files-map
        "C-w" 'helm-find-files-up-one-level
        "TAB" 'helm-execute-persistent-action)
  (mapc (lambda (r) (push r helm-boring-file-regexp-list))
        (list "\\.projects$" "\\.DS_Store$")))

(use-package helm-ag
  :commands (helm-ag
             helm-ag-mode
             helm-do-ag
             helm-do-ag-this-file
             helm-do-ag-project-root
             helm-do-ag-buffers
             helm-ag-project-root
             helm-ag-pop-stack
             helm-ag-buffers
             helm-ag-clear-stack)
  :config
  (map! :map helm-ag-map
        "<backtab>"  'helm-ag-edit
        :map helm-ag-edit-map
        "<escape>"   'helm-ag--edit-abort
        :n "zx"      'helm-ag--edit-abort))

(use-package helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment)
  :config
  (setq helm-css-scss-split-direction 'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))

(use-package helm-swoop ; https://github.com/ShingoFukuyama/helm-swoop
  :defines  (helm-swoop-last-prefix-number)
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))

(use-package helm-describe-modes :commands helm-describe-modes)
(use-package helm-ring :commands helm-show-kill-ring)
(use-package helm-semantic :commands helm-semantic-or-imenu)
(use-package helm-elisp :commands helm-apropos)
(use-package helm-command :commands helm-M-x)

(provide 'core-helm)
;;; core-helm.el ends here
