;;; completion/helm/config.el -*- lexical-binding: t; -*-

(defvar +helm-global-prompt "››› "
  "The helm text prompt prefix string is globally replaced with this string.")

(defvar +helm-project-search-engines '(rg ag pt)
  "What search tools for `+helm/project-search' (and `+helm-file-search' when no
ENGINE is specified) to try, and in what order.

To disable a particular tool, remove it from this list. To prioritize a tool
over others, move it to the front of the list. Later duplicates in this list are
silently ignored.

If you want to already use git-grep or grep, set this to nil.")

;; Posframe (requires +childframe)
(defvar +helm-posframe-handler
  #'+helm-poshandler-frame-center-near-bottom
  "The function that determines the location of the childframe. It should return
a cons cell representing the X and Y coordinates. See
`posframe-poshandler-frame-center' as a reference.")

(defvar +helm-posframe-font-scale 1
  "The text-scale to use in the helm childframe. Set to nil for no scaling. Can
be negative.")


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
    [remap projectile-find-file]      #'+helm/projectile-find-file
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
        helm-mode-handle-completion-in-region nil
        ;; default sizes
        helm-display-buffer-default-width nil
        helm-display-buffer-default-height 0.25)

  (when (and EMACS26+ (featurep! +childframe))
    (setq helm-display-function #'+helm-posframe-display
          helm-display-buffer-default-height 0.3)
    (advice-add #'posframe--get-font-height :override #'ignore))

  (let ((fuzzy (featurep! +fuzzy)))
    (setq helm-mode-fuzzy-match fuzzy
          helm-completion-in-region-fuzzy-match fuzzy
          helm-M-x-fuzzy-match fuzzy
          helm-apropos-fuzzy-match fuzzy
          helm-bookmark-show-location fuzzy
          helm-buffers-fuzzy-matching fuzzy
          helm-completion-in-region-fuzzy-match fuzzy
          helm-file-cache-fuzzy-match fuzzy
          helm-flx-for-helm-locate fuzzy
          helm-imenu-fuzzy-match fuzzy
          helm-apropos-fuzzy-match fuzzy
          helm-lisp-fuzzy-completion fuzzy
          helm-locate-fuzzy-match fuzzy
          helm-projectile-fuzzy-match fuzzy
          helm-recentf-fuzzy-match fuzzy
          helm-semantic-fuzzy-match fuzzy))

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
  :hook (helm-mode . helm-flx-mode)
  :config (helm-flx-mode +1))


;; `helm-ag'
(after! helm-ag
  (map! :map helm-ag-edit-map :n "RET" #'compile-goto-error)
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


;; `swiper-helm'
(setq swiper-helm-display-function
      (lambda (buf &optional _resume) (pop-to-buffer buf)))


(def-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


;;
;; Evil integration
;;

(when (featurep! :feature evil +everywhere)
  (setq helm-default-prompt-display-function #'+helm--set-prompt-display)

  (map! (:after helm
          :map helm-map
          "C-S-p" #'helm-previous-source
          "C-S-n" #'helm-next-source
          "C-l" #'helm-execute-persistent-action
          "C-j" #'helm-next-line
          "C-k" #'helm-previous-line
          "C-f" #'helm-next-page
          "C-u" #'helm-previous-page
          [tab] #'helm-select-action)
        (:after helm-files
          :map (helm-find-files-map helm-read-file-map)
          [M-return] #'helm-ff-run-switch-other-window
          "M-h" #'helm-find-files-up-one-level)
        (:after helm-locate
          :map helm-generic-files-map
          "S-<return>" #'helm-ff-run-switch-other-window)
        (:after helm-buffers
          :map helm-buffer-map
          [M-return] #'helm-buffer-switch-other-window
          [return]   #'display-buffer)
        (:after helm-regexp
          :map helm-moccur-map
          [M-return] #'helm-moccur-run-goto-line-ow)
        (:after helm-grep
          :map helm-grep-map
          [M-return] #'helm-grep-run-other-window-action)))
