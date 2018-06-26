;;; completion/helm/config.el -*- lexical-binding: t; -*-

(defvar +helm-global-prompt "››› "
  "The helm text prompt prefix string is globally replaced with this string.")


;;
;; Packages
;;

(def-package! helm-mode
  :defer 1
  :after-call pre-command-hook
  :init
  (define-key! 'global
    [remap apropos]                   #'helm-apropos
    [remap bookmark-jump]             #'helm-bookmarks
    [remap execute-extended-command]  #'helm-M-x
    [remap find-file]                 #'helm-find-files
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
  (add-to-list 'helm-completing-read-handlers-alist (cons #'find-file-at-point nil)))


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
  (defun +helm*replace-prompt (plist)
    "Globally replace helm prompts with `+helm-global-prompt'."
    (cond ((not +helm-global-prompt) plist)
          ((keywordp (car plist))
           (plist-put plist :prompt +helm-global-prompt))
          ((setf (nth 2 plist) +helm-global-prompt)
           plist)))
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


;; `helm-ag'
(after! helm-ag
  (define-key helm-ag-edit-map [remap quit-window] #'helm-ag--edit-abort)
  (set-popup-rule! "^\\*helm-ag-edit" :size 0.35 :ttl 0 :quit nil))


;; `helm-bookmark'
(setq helm-bookmark-show-location t)


;; `helm-css-scss' -- https://github.com/ShingoFukuyama/helm-css-scss
(setq helm-css-scss-split-direction #'split-window-vertically
      helm-css-scss-split-with-multiple-windows t)


;; `helm-files'
(after! helm-files
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


;; `helm-locate'
(defvar helm-generic-files-map (make-sparse-keymap))
(after! helm-locate (set-keymap-parent helm-generic-files-map helm-map))


;; `helm-projectile'
(def-package! helm-projectile
  :commands (helm-projectile-find-file
             helm-projectile-recentf
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (setq projectile-completion-system 'helm)
  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  :config
  (set-keymap-parent helm-projectile-find-file-map helm-map))


(def-package! helm-swoop ; https://github.com/ShingoFukuyama/helm-swoop
  :commands helm-multi-swoop-all
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        ;; no initial input
        helm-swoop-pre-input-function (lambda () "")
        ;; Always split below current window
        helm-swoop-split-with-multiple-windows t))


(def-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(def-package! posframe
  :when (and EMACS26+ (featurep! +childframe))
  :after helm
  :config
  (defvar +helm--posframe-buffer nil)

  (defun +helm-posframe-display (buffer &optional _resume)
    (posframe-show
      (setq +helm--posframe-buffer buffer)
      :poshandler #'posframe-poshandler-frame-bottom-left-corner
      :left-fringe 10
      :width (frame-width)
      :height 16 ;; ivy/+childframe uses 16
      :respect-header-line t))

  (defun +helm|posframe-cleanup ()
    (posframe-hide +helm--posframe-buffer))

  (add-hook 'helm-cleanup-hook #'+helm|posframe-cleanup)
  (setq helm-display-function #'+helm-posframe-display))


;;
;; Evil integration
;;

(when (featurep! :feature evil +everywhere)
  (setq helm-default-prompt-display-function #'+helm--set-prompt-display)

  (map! (:after helm
          :map helm-map
          :ni "C-S-p" #'helm-previous-source
          :ni "C-S-n" #'helm-next-source
          :ni "C-l" #'helm-execute-persistent-action
          :ni "C-j" #'helm-next-line
          :ni "C-k" #'helm-previous-line
          :ni "C-f" #'helm-next-page
          :ni "C-b" #'helm-previous-page
          :n  [tab] #'helm-select-action  ; TODO: Ivy has "ga".
          :n  "["  #'helm-previous-source
          :n  "]"  #'helm-next-source
          :n  "gk" #'helm-previous-source
          :n  "gj" #'helm-next-source
          :n  "("  #'helm-prev-visible-mark
          :n  ")"  #'helm-next-visible-mark
          :n  "j"  #'helm-next-line
          :n  "k"  #'helm-previous-line
          :n  "gg" #'helm-beginning-of-buffer
          :n  "G"  #'helm-end-of-buffer
          :n  "/"  #'helm-quit-and-find-file
          :n  "gr" #'helm-refresh
          :n  "yp" #'helm-yank-selection
          :n  "yP" #'helm-copy-to-buffer
          :n  "yy" #'helm-kill-selection-and-quit)
        (:after helm-files
          :map (helm-find-files-map helm-read-file-map)
          :n  "go" #'helm-ff-run-switch-other-window
          :n  "/"  #'helm-ff-run-find-sh-command
          :ni "M-<return>" #'helm-ff-run-switch-other-window
          :ni "M-h" #'helm-find-files-up-one-level
          :n  "="  #'helm-ff-run-ediff-file
          :n  "%"  #'helm-ff-run-query-replace-regexp
          :n  "D"  #'helm-ff-run-delete-file) ; Ivy has "D".
        (:after helm-locate
          :map helm-generic-files-map
          :n  "go" #'helm-ff-run-switch-other-window
          :ni "S-<return>" #'helm-ff-run-switch-other-window)
        (:after helm-buffers
          :map helm-buffer-map
          :n  "go" #'helm-buffer-switch-other-window
          :n  "gO" #'display-buffer
          :ni "M-<return>" #'helm-buffer-switch-other-window
          :ni "<return>" #'display-buffer
          :n  "=" #'helm-buffer-run-ediff
          :n  "%" #'helm-buffer-run-query-replace-regexp
          :n  "D" #'helm-buffer-run-kill-persistent) ; Ivy has "D".
        (:after helm-regexp
          :map helm-moccur-map
          :n  "go" #'helm-moccur-run-goto-line-ow
          :ni "M-<return>" #'helm-moccur-run-goto-line-ow)
        (:after helm-grep
          :map helm-grep-map
          :n  "go" #'helm-grep-run-other-window-action
          :ni "M-<return>" #'helm-grep-run-other-window-action)))
