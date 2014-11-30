(provide 'init-project)

(use-package neotree
  :commands (neotree-show neotree-hide neotree-toggle))

(use-package ag
  :commands (ag ag-search ag-regexp)
  :config
  (progn (setq ;ag-reuse-window nil
               ag-reuse-buffers nil
               ag-highlight-search t)

         (push '(ag-mode :position bottom :height 0.5 :stick t) popwin:special-display-config)

         ;; Close Ag on ESC
         (bind ag-mode-map [escape] "q")))

(use-package dired
  :init
  (progn (setq dired-recursive-deletes 'always
               dired-recursive-copies 'always
               dired-auto-revert-buffer t

               ;; if there is a dired buffer displayed in the next
               ;; window, use its current subdir, instead of the
               ;; current subdir of this dired buffer
               dired-dwim-target t)

         (push '(dired-mode :position bottom :height 0.5 :stick t) popwin:special-display-config)

         (add-hook! 'dired-mode-hook
                    (use-package 'dired+ :config (toggle-diredp-find-file-reuse-dir 1)))))

(use-package projectile
  :init
  (progn (setq-default projectile-cache-file (concat *tmp-dir "projectile.cache"))
         (setq-default projectile-known-projects-file (concat *tmp-dir "projectile.projects"))
         (setq-default projectile-enable-caching t)

         (add-to-list 'projectile-globally-ignored-files "ido.last")
         (add-to-list 'projectile-globally-ignored-directories "assets")
         (add-to-list 'projectile-other-file-alist '("scss" "css"))
         (add-to-list 'projectile-other-file-alist '("css" "scss"))

         (projectile-global-mode +1)

         (defvar persp-modestring-dividers '("" " |" ","))
         (use-package perspective)
         (use-package persp-projectile)
         (persp-mode 1)))

(use-package helm
  :pre-load (defvar helm-mode-line-string "")
  :init
  (progn
    (use-package helm-ag :commands (helm-do-ag))
    (use-package helm-projectile)
    (add-hook! 'scss-mode-hook (use-package helm-css-scss))

    (push '("^\\*helm.+\\*$" :position bottom :regexp t :height 18) popwin:special-display-config)

    (setq helm-quick-update t
          helm-idle-delay 0.01
          helm-input-idle-delay 0.01)

    (bind helm-map (kbd "C-w") 'evil-delete-backward-word)

    ;; All this for a smaller prompt (it was redundant with helm headers)
    (defmacro helm-projectile-command (command source prompt)
      `(defun ,(intern (concat "helm-projectile-" command)) (&optional arg)
         (interactive "P")
         (if (projectile-project-p)
             (projectile-maybe-invalidate-cache arg))
         (let ((helm-ff-transformer-show-only-basename nil)
               ;; for consistency, we should just let Projectile take care of ignored files
               (helm-boring-file-regexp-list nil))
           (helm :sources ,source
                 :buffer "*helm projectile*"
                 :prompt ">>> "))))

    (helm-projectile-command "switch-project" 'helm-source-projectile-projects ">>> ")
    (helm-projectile-command "find-file" helm-source-projectile-files-and-dired-list ">>> ")
    (helm-projectile-command "find-file-in-known-projects" 'helm-source-projectile-files-in-all-projects-list ">>> ")
    (helm-projectile-command "find-file-dwim" 'helm-source-projectile-files-dwim-list ">>> ")
    (helm-projectile-command "find-dir" helm-source-projectile-directories-and-dired-list ">>> ")
    (helm-projectile-command "recentf" 'helm-source-projectile-recentf-list ">>> ")
    (helm-projectile-command "switch-to-buffer" 'helm-source-projectile-buffers-list ">>> ")

    ;; Hide the mode-line in helm (<3 minimalism)
    (defun helm-display-mode-line (source &optional force)
      "Setup mode-line and header-line for `helm-buffer'."
      (set (make-local-variable 'helm-mode-line-string)
           (helm-interpret-value (or (and (listp source) ; Check if source is empty.
                                          (assoc-default 'mode-line source))
                                     (default-value 'helm-mode-line-string))
                                 source))
      (let ((follow (and (eq (cdr (assq 'follow source)) 1) "(HF) ")))
        ;; Setup mode-line.
        (if helm-mode-line-string
            (setq mode-line-format nil)
          (setq mode-line-format (default-value 'mode-line-format)))
        ;; Setup header-line.
        (let* ((hlstr (helm-interpret-value
                       (and (listp source)
                            (assoc-default 'header-line source))
                       source))
               (hlend (make-string (max 0 (- (window-width) (length hlstr))) ? )))
          (setq header-line-format
                (propertize (concat " " hlstr hlend) 'face 'helm-header))))
      (when force (force-mode-line-update)))

    ;; No persistent header
    (defadvice helm-display-mode-line (after undisplay-header activate)
      (setq header-line-format nil))))

;; For setting project-specific settings
(defmacro project-settings (name &rest body)
  (declare (indent 1))
  `(progn
     (add-hook 'find-file-hook
               (lambda ()
                 (when (string-match-p ,name (buffer-file-name))
                   ,@body)))
     (add-hook 'dired-after-readin-hook
               (lambda ()
                 (when (string-match-p ,name (dired-current-directory))
                   ,@body)))))
