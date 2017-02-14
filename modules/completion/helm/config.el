;;; completion/helm/config.el

;; TODO Untested in DOOM v2.0

(defvar +helm-global-prompt "››› "
  "The helm text prompt prefix string is globally replaced with this string.")

(@map [remap find-file] 'helm-find-files
      [remap switch-to-buffer] 'doom/helm-buffers-dwim
      [remap projectile-switch-to-buffer] (λ! (doom/helm-buffers-dwim t))
      [remap recentf]            'helm-recentf
      [remap projectile-recentf] 'helm-projectile-recentf
      [remap projectile-find-file] 'helm-projectile-find-file
      [remap imenu]              'helm-semantic-or-imenu
      [remap bookmark-jump]      'helm-bookmarks
      [remap noop-show-kill-ring] 'helm-show-kill-ring
      [remap projectile-switch-project] 'helm-projectile-switch-project
      [remap projectile-find-file] 'helm-projectile-find-file
      [remap imenu-anywhere]  'helm-imenu-anywhere
      [remap execute-extended-command] 'helm-M-x)


;;
;; Packages
;;

(@def-package helm
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
  (setq projectile-completion-system 'helm)

  (@map "M-x"         'helm-M-x
        "A-x"         'helm-M-x
        "M-X"         'helm-apropos
        "A-X"         'helm-apropos
        "M-o"         'helm-find-files

        (:map helm-map
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
          "<tab>"      'helm-execute-persistent-action)

        (:map* helm-generic-files-map
          :e "ESC"     'helm-keyboard-quit))

  ;;; Popup setup
  (@set :popup "\\` ?\\*[hH]elm.*?\\*\\'" :size 14 :regexp t)

  ;;; Helm hacks
  (defconst doom-helm-header-fg (face-attribute 'helm-source-header :foreground))
  ;; Shrink source headers if there is only one source
  (add-hook 'helm-after-initialize-hook 'doom*helm-hide-source-header-maybe)
  ;; A simpler prompt: see `+helm-global-prompt'
  (advice-add 'helm :filter-args 'doom*helm-replace-prompt)
  ;; Hide mode-line in helm windows
  (advice-add 'helm-display-mode-line :override 'doom*helm-hide-header)

  (require 'helm-mode)
  (helm-mode +1)

  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (require 'helm-projectile)
  (set-keymap-parent helm-projectile-find-file-map helm-map))


(@def-package helm-locate
  :init
  (defvar helm-generic-files-map (make-sparse-keymap)
    "Generic Keymap for files.")
  :config (set-keymap-parent helm-generic-files-map helm-map))


(@def-package helm-buffers
  :commands (helm-buffers-list helm-mini)
  :config (advice-add 'helm-buffer-list :override 'helm*buffer-list))


(@def-package helm-tags
  :commands (helm-tags-get-tag-file helm-etags-select))


(@def-package helm-bookmark
  :commands (helm-bookmarks helm-filtered-bookmarks)
  :config (setq-default helm-bookmark-show-location t))


(@def-package helm-files
  :commands (helm-browse-project helm-find helm-find-files helm-for-files helm-multi-files helm-recentf)
  :config
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list))

  (@map :map helm-find-files-map
        "C-w" 'helm-find-files-up-one-level
        "TAB" 'helm-execute-persistent-action))


(@def-package helm-ag
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
  (@map (:map helm-ag-map
          "<backtab>"  'helm-ag-edit)
        (:map helm-ag-edit-map
          "<escape>"   'helm-ag--edit-abort
          "C-c C-c"    'helm-ag--edit-commit
          :n "zx"      'helm-ag--edit-abort)))


(@def-package helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment)
  :config
  (setq helm-css-scss-split-direction 'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))


(@def-package helm-swoop ; https://github.com/ShingoFukuyama/helm-swoop
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))


(@def-package helm-describe-modes :commands helm-describe-modes)
(@def-package helm-ring :commands helm-show-kill-ring)
(@def-package helm-semantic :commands helm-semantic-or-imenu)
(@def-package helm-elisp :commands helm-apropos)
(@def-package helm-command :commands helm-M-x)


;;
;; Popup hacks
;;

(@after helm
  ;; Helm tries to clean up after itself, but shackle has already done this.
  ;; This fixes that. To reproduce, add a helm rule in `shackle-rules', open two
  ;; splits side-by-side, move to the buffer on the right and invoke helm. It
  ;; will close all but the left-most buffer.
  (setq-default helm-reuse-last-window-split-state t
                helm-split-window-in-side-p t))

(@after helm-swoop
  (setq helm-swoop-split-window-function (lambda (b) (doom-popup-buffer b))))

(@after helm-ag
  ;; This prevents helm-ag from switching between windows and buffers.
  (defadvice helm-ag--edit-abort (around helm-ag-edit-abort-popup-compat activate)
    (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it)
    (doom/popup-close nil t))

  (defadvice helm-ag--edit-commit (around helm-ag-edit-commit-popup-compat activate)
    (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it)
    (doom/popup-close nil t))

  (defadvice helm-ag--edit (around helm-ag-edit-popup-compat activate)
    (cl-letf (((symbol-function 'other-window) 'ignore)
              ((symbol-function 'switch-to-buffer) 'doom-popup-buffer))
      ad-do-it
      (with-current-buffer (get-buffer "*helm-ag-edit*")
        (use-local-map helm-ag-edit-map)))))

