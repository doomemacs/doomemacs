;;; completion/helm/config.el -*- lexical-binding: t; -*-

;; Warning: since I don't use helm, this may be out of date.

(defvar +helm-global-prompt "››› "
  "The helm text prompt prefix string is globally replaced with this string.")


;;
;; Packages
;;

(def-package! helm-mode
  :defer 1
  :after-call pre-command-hook
  :init
  (map! :map global-map
        [remap apropos]                   #'helm-apropos
        [remap bookmark-jump]             #'helm-bookmarks
        [remap bookmark-jump]             #'helm-bookmarks
        [remap execute-extended-command]  #'helm-M-x
        [remap find-file]                 #'helm-find-files
        [remap imenu-anywhere]            #'helm-imenu-anywhere
        [remap imenu-anywhere]            #'helm-imenu-anywhere
        [remap imenu]                     #'helm-semantic-or-imenu
        [remap noop-show-kill-ring]       #'helm-show-kill-ring
        [remap projectile-find-file]      #'helm-projectile-find-file
        [remap projectile-recentf]        #'helm-projectile-recentf
        [remap projectile-switch-project] #'helm-projectile-switch-project
        [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
        [remap recentf-open-files]        #'helm-recentf)
  :config
  (helm-mode +1)
  ;; helm is too heavy for find-file-at-point
  (map-put helm-completing-read-handlers-alist 'find-file-at-point nil))


(def-package! helm
  :after helm-mode
  :init
  (setq helm-candidate-number-limit 50
        ;; Display extraineous helm UI elements
        helm-display-header-line nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Don't override evil-ex's completion
        helm-mode-handle-completion-in-region nil)

  :config
  (setq projectile-completion-system 'helm)

  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (require 'helm-projectile)
  (set-keymap-parent helm-projectile-find-file-map helm-map)

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

  (defun +helm*hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-current-buffer (helm-buffer-get) helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook #'+helm*hide-minibuffer-maybe))


(def-package! helm-flx
  :when (featurep! +fuzzy)
  :after helm
  :init
  (setq helm-candidate-number-limit 40
        helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-bookmark-show-location t
        helm-buffers-fuzzy-matching t
        helm-completion-in-region-fuzzy-match t
        helm-file-cache-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-flx-for-helm-locate t
        helm-mode-fuzzy-match t
        helm-projectile-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t)
  :config
  (helm-flx-mode +1))


(def-package! helm-locate
  :defer t
  :init (defvar helm-generic-files-map (make-sparse-keymap))
  :config (set-keymap-parent helm-generic-files-map helm-map))


(after! helm-bookmark
  (setq-default helm-bookmark-show-location t))


(after! helm-files
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


;; `helm-ag'
(map! :after helm-ag
      :map helm-ag-edit-map [remap quit-window] #'helm-ag--edit-abort)


(after! helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  (setq helm-css-scss-split-direction #'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))


(def-package! helm-swoop ; https://github.com/ShingoFukuyama/helm-swoop
  :commands helm-multi-swoop-all
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))


(def-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))
