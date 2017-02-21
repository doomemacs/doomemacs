;;; completion/ivy/packages.el

;; TODO Make this a setting
(defmacro @def-counsel-action (name &rest forms)
  `(defun ,(intern (format "+ivy/counsel-%s" (symbol-name name))) ()
     (interactive)
     (ivy-set-action ',@forms)
     (setq ivy-exit 'done)
     (exit-minibuffer)))

(@map [remap find-file] 'counsel-find-file
      [remap switch-to-buffer] 'ivy-switch-buffer
      [remap projectile-switch-to-buffer] '+ivy/switch-project-buffer
      [remap recentf] 'counsel-recentf
      [remap imenu] 'counsel-imenu
      [remap bookmark-jump] 'counsel-bookmark
      [remap projectile-switch-project] 'counsel-projectile-switch-project
      [remap projectile-find-file] 'counsel-projectile-find-file
      [remap imenu-anywhere]  'ivy-imenu-anywhere
      [remap execute-extended-command] 'counsel-M-x)


;;
;; Packages
;;

(@def-package ivy :demand t
  :init
  (setq ivy-height 14
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-format-function 'ivy-format-function-line) ;; highlight til EOL

  :config
  (setq projectile-completion-system 'ivy
        smex-completion-method 'ivy)

  (@map :map ivy-mode-map
        [remap ivy-switch-buffer] '+ivy/switch-buffer
        [remap projectile-switch-to-buffer] '+ivy/switch-project-buffer

        :map ivy-minibuffer-map
        [escape] 'keyboard-escape-quit
        "C-r" 'evil-paste-from-register
        "M-v" 'clipboard-yank
        "C-w" 'backward-kill-word
        "C-u" 'backward-kill-sentence
        "C-b" 'backward-word
        "C-f" 'forward-word)

  ;; Occasionally, when ivy closes, it causes display artifacting
  ;; between horizontal splits. This fixes it, though may cause
  ;; flickering on some OSes.
  (defun doom|redisplay (&rest _) (force-mode-line-update))
  (advice-add 'ivy-read :after 'doom|redisplay)
  (add-hook 'projectile-find-file-hook 'doom|redisplay)

  (@after magit      (setq magit-completing-read-function 'ivy-completing-read))
  (@after yasnippet  (push '+ivy-yas-prompt yas-prompt-functions))

  (ivy-mode +1))


(@def-package counsel
  :after ivy
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  (@set :popup "^\\*ivy-occur counsel-ag" :size 25 :select t :regexp t)

  (require 'counsel-projectile)

  (@def-counsel-action ag-open-in-other-window
    (lambda (x)
      (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
        (let ((file-name (match-string-no-properties 1 x))
              (line-number (match-string-no-properties 2 x)))
          (with-ivy-window
            (find-file-other-window (expand-file-name file-name counsel--git-grep-dir))
            (forward-line (1- (string-to-number line-number)))
            (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
            (swiper--ensure-visible)
            (run-hooks 'counsel-grep-post-action-hook)
            (unless (eq ivy-exit 'done)
              (swiper--cleanup)
              (swiper--add-overlays (ivy--regex ivy-text))))))))

  (@def-counsel-action open-in-other-window
    (lambda (x) (with-ivy-window (find-file-other-window x))))

  (@add-hook doom-popup-mode
    (when (eq major-mode 'ivy-occur-grep-mode)
      (ivy-wgrep-change-to-wgrep-mode)))

  (advice-add 'counsel-ag-function :override '+ivy*counsel-ag-function)
  (@map :map counsel-ag-map
        [backtab] '+ivy/counsel-ag-occur     ; search/replace on results
        "C-SPC"   'counsel-git-grep-recenter ; preview
        "M-RET"   '+ivy/counsel-ag-open-in-other-window))


;; Used by `counsel-M-x'
(@def-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat doom-cache-dir "/smex-items"))
  (smex-initialize))

