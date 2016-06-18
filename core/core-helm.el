;;; core-helm.el

(use-package helm
  :init
  (setq helm-quick-update t
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

  :config
  (defvar helm-global-prompt "››› ")

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

  ;;; Popup setup
  (def-popup! "\\` ?\\*[hH]elm.*?\\*\\'" :align below :size 14 :select t :regexp t)

  ;;; Helm hacks
  (defconst doom-helm-header-fg (face-attribute 'helm-source-header :foreground))
  ;; Shrink source headers if there is only one source
  (add-hook 'helm-after-initialize-hook 'doom*helm-hide-source-header-maybe)
  ;; A simpler prompt: see `helm-global-prompt'
  (advice-add 'helm :filter-args 'doom*helm-replace-prompt)
  ;; Hide mode-line in helm windows
  (advice-add 'helm-display-mode-line :override 'doom*helm-hide-header)

  (require 'helm-mode)
  (helm-mode +1))

(use-package helm-locate
  :defer t
  :init
  (defvar helm-generic-files-map (make-sparse-keymap)
    "Generic Keymap for files.")
  :config (set-keymap-parent helm-generic-files-map helm-map))

(use-package helm-buffers
  :commands (helm-buffers-list helm-mini)
  :config (advice-add 'helm-buffer-list :override 'helm*buffer-list))

(use-package helm-tags
  :commands (helm-tags-get-tag-file helm-etags-select))

(use-package helm-bookmark
  :commands (helm-bookmarks helm-filtered-bookmarks)
  :config (setq-default helm-bookmark-show-location t))

(use-package helm-projectile
  :commands (helm-projectile-find-other-file
             helm-projectile-switch-project
             helm-projectile-find-file
             helm-projectile-find-dir)
  :init
  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  :config
  (set-keymap-parent helm-projectile-find-file-map helm-map)
  (setq projectile-completion-system 'helm))

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


;; Popup hacks
(after! helm
  ;; This is a good alternative to either popwin or shackle, specifically for
  ;; helm. If either fail me (for the last time), this is where I'll turn.
  ;;(add-to-list 'display-buffer-alist
  ;;             `(,(rx bos "*helm" (* not-newline) "*" eos)
  ;;               (display-buffer-in-side-window)
  ;;               (inhibit-same-window . t)
  ;;               (window-height . 0.4)))

  ;; Helm tries to clean up after itself, but shackle has already done this.
  ;; This fixes that. To reproduce, add a helm rule in `shackle-rules', open two
  ;; splits side-by-side, move to the buffer on the right and invoke helm. It
  ;; will close all but the left-most buffer.
  (setq-default helm-reuse-last-window-split-state t
                helm-split-window-in-side-p t))

(after! helm-swoop
  (setq helm-swoop-split-window-function (lambda (b) (doom/popup-buffer b))))

(after! helm-ag
  ;; This prevents helm-ag from switching between windows and buffers.
  (defadvice helm-ag--edit-abort (around helm-ag-edit-abort-popup-compat activate)
    (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it)
    (doom/popup-close nil t t))
  (defadvice helm-ag--edit-commit (around helm-ag-edit-commit-popup-compat activate)
    (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it)
    (doom/popup-close nil t t))
  (defadvice helm-ag--edit (around helm-ag-edit-popup-compat activate)
    (cl-letf (((symbol-function 'other-window) 'ignore)
              ((symbol-function 'switch-to-buffer) 'doom/popup-buffer))
      ad-do-it)))

(provide 'core-helm)
;;; core-helm.el ends here
