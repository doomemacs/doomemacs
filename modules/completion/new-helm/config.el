;;; completion/helm/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! helm-mode
  :config
  (when (featurep! +find-file-at-point)
    ;; helm is too heavy for `find-file-at-point' (is this still true?)
    (add-to-list 'helm-completing-read-handlers-alist (cons #'find-file-at-point nil))))


(use-package! helm
  :after helm-mode
  :init
  (customize-set-variable 'helm-multi-files-toggle-locate-binding "C-c L")

  :config
  (setq helm-candidate-number-limit 150
        ;; Remove extraineous helm UI elements
        helm-display-header-line nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; When calling `helm-semantic-or-imenu', don't immediately jump to
        ;; symbol at point
        helm-imenu-execute-action-at-once-if-one nil)

  (when (and (featurep! +ack-grep) (executable-find "ack"))
    (setq helm-grep-default-command
       "ack -Hn --color --smart-case --no-group -- %p %f"
       helm-grep-default-recurse-command
       "ack -H --color --smart-case --no-group -- %p %f"))

  (when (featurep! +disable-helm-ff-lynx-style)
    ;; disable special behavior for left/right, M-left/right keys.
    (setq helm-ff-lynx-style-map nil))

  (when (featurep! +remap-commands)
    (map! [remap apropos]                   #'helm-apropos
          [remap find-library]              #'helm-locate-library
          [remap bookmark-jump]             #'helm-bookmarks
          [remap execute-extended-command]  #'helm-M-x
          [remap find-file]                 #'helm-find-files
          [remap ibuffer-find-file]         #'helm-find-files
          [remap locate]                    #'helm-locate
          [remap imenu]                     #'helm-semantic-or-imenu
          [remap noop-show-kill-ring]       #'helm-show-kill-ring
          [remap persp-switch-to-buffer]    #'+helm/workspace-mini
          [remap switch-to-buffer]          #'helm-buffers-list
          [remap projectile-find-file]      #'+helm/projectile-find-file
          [remap projectile-recentf]        #'helm-projectile-recentf
          [remap projectile-switch-project] #'helm-projectile-switch-project
          [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
          [remap recentf-open-files]        #'helm-recentf
          [remap yank-pop]                  #'helm-show-kill-ring))

  (when (featurep! +childframe)
    (setq helm-posframe-poshandler #'posframe-poshandler-frame-center)
    (setq helm-posframe-width 0.65)
    (setq helm-posframe-height 0.35)
    (setq helm-posframe-min-width 80)
    (setq helm-posframe-min-height 16)
    (setq helm-posframe-border-width 8)
    (helm-posframe-enable))

  (when (featurep! +autoresize)
    (unless (featurep! +helm-popup-layout)
      (progn
        (setq helm-split-window-inside-p t)
        (helm-autoresize-mode t))))

  (when (featurep! :editor evil +everywhere)
    (setq helm-default-prompt-display-function #'+helm--set-prompt-display))

  (let ((fuzzy (featurep! +fuzzy)))
    (setq helm-apropos-fuzzy-match fuzzy
          helm-bookmark-show-location fuzzy
          helm-buffers-fuzzy-matching fuzzy
          helm-ff-fuzzy-matching fuzzy
          helm-file-cache-fuzzy-match fuzzy
          helm-flx-for-helm-locate fuzzy
          helm-imenu-fuzzy-match fuzzy
          helm-lisp-fuzzy-completion fuzzy
          helm-locate-fuzzy-match fuzzy
          helm-projectile-fuzzy-match fuzzy
          helm-recentf-fuzzy-match fuzzy
          helm-semantic-fuzzy-match fuzzy)

    ;; Make sure that we have helm-multi-matching or fuzzy matching,
    ;; (as prescribed by the fuzzy flag) also in the following cases:
    ;;
    ;; - helmized commands that use `completion-at-point' and similar functions
    ;; - native commands that fall back to `completion-styles' like `helm-M-x'
    ;;
    ;; However, do not add helm completion styles to the front of
    ;; `completion-styles', since that would be overly intrusive. E.g., it
    ;; results in `company-capf' returning far to many completion candidates.
    ;; Instead, append those styles so that they act as a fallback.
    ;; Variable completion-styles is ignored unless helm-completion-style is
    ;; customized to 'emacs.
    (customize-set-variable 'helm-completion-style 'emacs)
    (if (featurep! :completion vertico)
        (if fuzzy
            (after! vertico
              (add-to-list 'completion-styles 'flex t))
          (after! vertico
            (add-to-list 'completion-styles 'helm t)))
      (add-to-list 'completion-styles (if fuzzy 'flex 'helm) t)))

  (when (featurep! +helm-popup-layout)
    (setq helm-display-buffer-default-height 0.25)
    (set-popup-rule! "^\\*helm" :vslot -100 :size 0.22 :ttl nil))

  (when (featurep! +helm-hide-mode-line)
    ;; Hide the modeline in helm windows.
    (setq helm-mode-line-string nil)
    (defun +helm--hide-mode-line (&rest _)
      (with-current-buffer (helm-buffer-get)
        (unless helm-mode-line-string
          (hide-mode-line-mode +1))))
    (add-hook 'helm-after-initialize-hook #'+helm--hide-mode-line)
    (advice-add #'helm-display-mode-line :override #'+helm--hide-mode-line)
    (advice-add #'helm-ag-show-status-default-mode-line :override #'ignore))

  ;; Use helpful instead of describe-* to display documentation
  (dolist (fn '(helm-describe-variable helm-describe-function))
    (advice-add fn :around #'doom-use-helpful-a))

  (when (featurep! +helm-mode)
    (helm-mode 1)))


(use-package! helm-flx
  :when (featurep! +fuzzy)
  :config (helm-flx-mode +1))


(after! helm-rg
  (when (featurep! +helm-popup-layout)
    (setq helm-rg-display-buffer-normal-method #'pop-to-buffer)
    (set-popup-rule! "^helm-rg-" :ttl nil :select t :size 0.45))
  (map! :map helm-rg-map
        "C-c C-e" #'helm-rg--bounce)
  (map! :map helm-rg--bounce-mode-map
        "q" #'kill-current-buffer
        "C-c C-c" (cmd! (helm-rg--bounce-dump) (kill-current-buffer))
        "C-x C-c" #'helm-rg--bounce-dump-current-file
        "C-c C-k" #'kill-current-buffer))


;;;###package helm-bookmark
(setq helm-bookmark-show-location t)


(after! helm-files
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


(defvar helm-generic-files-map (make-sparse-keymap))
(after! helm-locate
  (when (and IS-MAC
             (null helm-locate-command)
             (executable-find "mdfind"))
    (setq helm-locate-command "mdfind -name %s"))
  (set-keymap-parent helm-generic-files-map helm-map))


(use-package! helm-org
  :when (featurep! :lang org)
  :defer t
  :init
  (after! helm-mode
    (pushnew! helm-completing-read-handlers-alist
              '(org-capture . helm-org-completing-read-tags)
              '(org-set-tags . helm-org-completing-read-tags))))


(use-package! helm-projectile
  :after helm
  :commands (helm-projectile-find-file
             helm-projectile-recentf
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  :config
  (set-keymap-parent helm-projectile-find-file-map helm-map))


(after! swiper-helm
  (when (featurep! +helm-popup-layout)
    (setq ivy-height 20) ; for `swiper-isearch'
    (setq swiper-helm-display-function
          (lambda (buf &optional _resume) (pop-to-buffer buf))))
  (when (featurep! +remap-swiper)
    (global-set-key [remap swiper] #'swiper-helm))
  (add-to-list 'swiper-font-lock-exclude #'+doom-dashboard-mode nil #'eq))


(use-package! helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode))


(use-package! helm-icons
  :after helm
  :when (featurep! +icons)
  :init
  (if (featurep! +treemacs-icons)
      (setq helm-icons-provider 'treemacs)
    (progn
      (customize-set-value 'helm-icons-mode->icon nil) ; Fix for all-the-icons
      (setq helm-icons-provider 'all-the-icons)))
  :config
  (helm-icons-enable))
