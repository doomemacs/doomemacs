;;; completion/helm/config.el -*- lexical-binding: t; -*-

;; Warning: since I don't use helm, this may be out of date.

(defvar +helm-global-prompt "››› "
  "The helm text prompt prefix string is globally replaced with this string.")


;;
;; Packages
;;

(def-package! helm
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
  (add-hook 'doom-init-hook #'helm-mode)

  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (require 'helm-projectile)
  (set-keymap-parent helm-projectile-find-file-map helm-map)

  ;; helm is too heavy for find-file-at-point
  (after! helm-mode
    (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil)))

  (setq projectile-completion-system 'helm)

  ;;; Helm hacks
  (defun +helm*replace-prompt (plist)
    "Globally replace helm prompts with `+helm-global-prompt'."
    (if (keywordp (car plist))
        (plist-put plist :prompt +helm-global-prompt)
      (setf (nth 2 plist) +helm-global-prompt)
      plist))
  (advice-add #'helm :filter-args #'+helm*replace-prompt)

  (defun +helm*hide-header (&rest _)
    "Hide header-line & mode-line in helm windows."
    (setq mode-line-format nil))
  (advice-add #'helm-display-mode-line :override #'+helm*hide-header)

  (map! :map global-map
        [remap apropos]                   #'helm-apropos
        [remap find-file]                 #'helm-find-files
        [remap recentf-open-files]        #'helm-recentf
        [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
        [remap projectile-recentf]        #'helm-projectile-recentf
        [remap projectile-find-file]      #'helm-projectile-find-file
        [remap imenu]                     #'helm-semantic-or-imenu
        [remap bookmark-jump]             #'helm-bookmarks
        [remap noop-show-kill-ring]       #'helm-show-kill-ring
        [remap projectile-switch-project] #'helm-projectile-switch-project
        [remap projectile-find-file]      #'helm-projectile-find-file
        [remap imenu-anywhere]            #'helm-imenu-anywhere
        [remap execute-extended-command]  #'helm-M-x)
  (set! :popup "^\\*?helm" :ignore))


(def-package! helm-locate
  :defer t
  :init (defvar helm-generic-files-map (make-sparse-keymap))
  :config (set-keymap-parent helm-generic-files-map helm-map))


(def-package! helm-bookmark
  :commands helm-bookmark
  :config (setq-default helm-bookmark-show-location t))


(def-package! helm-files
  :defer t
  :config
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


(def-package! helm-ag
  :defer t
  :config
  (map! :map helm-ag-edit-map [remap quit-window] #'helm-ag--edit-abort))


(def-package! helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment)
  :config
  (setq helm-css-scss-split-direction #'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))


(def-package! helm-swoop ; https://github.com/ShingoFukuyama/helm-swoop
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))


(def-package! helm-describe-modes :commands helm-describe-modes)


(def-package! wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config (setq wgrep-auto-save-buffer t))
