(use-package helm
  :config
  (progn ; helm settings
    (defvar helm-global-prompt ">>> ")

    (setq helm-quick-update t
          helm-idle-delay 0.01
          helm-input-idle-delay 0.01
          helm-reuse-last-window-split-state t
          helm-buffers-fuzzy-matching nil
          helm-bookmark-show-location t)

    (my--cleanup-buffers-add "^\\*[Hh]elm.*\\*$")

    (evil-ex-define-cmd "a" 'helm-projectile-find-other-file)
    (evil-ex-define-cmd "proj[ect]" 'helm-projectile-switch-project)
    (evil-ex-define-cmd "ag" 'my:helm-ag-search)
    (evil-ex-define-cmd "agr" 'my:helm-ag-regex-search)
    (evil-ex-define-cmd "ag[cw]d" 'my:helm-ag-search-cwd)
    (evil-ex-define-cmd "agr[cw]d" 'my:helm-ag-regex-search-cwd)
    (evil-ex-define-cmd "sw[oop]" 'my:helm-swoop)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (use-package helm-ag
      :commands (my:helm-ag-search
                 my:helm-ag-search-cwd
                 my:helm-ag-regex-search
                 my:helm-ag-regex-search-cwd
                 helm-ag helm-do-ag)
      :config
      (progn
        ;; Ex-mode interface for `helm-ag'. If `bang', then `search' is interpreted as
        ;; regexp.
        (evil-define-operator my:helm-ag-search (beg end &optional search hidden-files-p pwd-p regex-p)
          :type inclusive
          :repeat nil
          (interactive "<r><a><!>")
          (let* ((helm-ag-default-directory (my--project-root pwd-p))
                 (helm-ag-command-option (concat (unless regex-p "-Q ")
                                                 (if hidden-files-p "--hidden ")))
                 (input "")
                 (header-name (format "Search in %s" helm-ag-default-directory)))
            (if search
                (progn
                  (helm-attrset 'search-this-file nil helm-ag-source)
                  (setq helm-ag--last-query search))
              (helm-ag-save-current-context)
              (if (and beg end (/= beg (1- end)))
                  (setq input (buffer-substring-no-properties beg end))))
            (helm-attrset 'name header-name helm-ag-source)
            (helm :sources (if search (helm-ag--select-source) '(helm-source-do-ag))
                  :buffer "*helm-ag*"
                  :input input
                  :prompt helm-global-prompt)))

        (evil-define-operator my:helm-ag-regex-search (beg end &optional bang search)
          :type inclusive
          :repeat nil
          (interactive "<r><!><a>")
          (my:helm-ag-search beg end search bang nil t))

        ;; Ex-mode interface for `helm-do-ag'. If `bang', then `search' is interpreted
        ;; as regexp
        (evil-define-operator my:helm-ag-search-cwd (beg end &optional search bang)
          :type inclusive
          :repeat nil
          (interactive "<r><a><!>")
          (my:helm-ag-search beg end search bang t nil))

        (evil-define-operator my:helm-ag-regex-search-cwd (beg end &optional search bang)
          :type inclusive
          :repeat nil
          (interactive "<r><a><!>")
          (my:helm-ag-search beg end search bang t t))))

    (use-package helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
      :commands (helm-css-scss
                 helm-css-scss-multi
                 helm-css-scss-insert-close-comment))

    (use-package helm-swoop    ; https://github.com/ShingoFukuyama/helm-swoop
      :commands (my:helm-swoop helm-swoop helm-multi-swoop)
      :config
      (progn
        (setq helm-swoop-use-line-number-face t
              helm-swoop-split-with-multiple-windows t
              helm-swoop-speed-or-color t
              helm-swoop-split-window-function 'popwin:popup-buffer)

        ;; Ex-mode interface for `helm-swoop', `helm-multi-swoop-all' (if `bang'), or
        ;; `helm-css-scss' and `helm-css-scss-multi' (if `bang') if major-mode is
        ;; `scss-mode'
        (evil-define-command my:helm-swoop (&optional search bang)
          :repeat nil
          (interactive "<a><!>")
          (if (eq major-mode 'scss-mode)
              (if bang (helm-css-scss-multi search) (helm-css-scss search))
            (if bang (helm-multi-swoop-all search) (helm-swoop :$query search))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (after "winner"
      ;; Tell winner-mode to ignore helm buffers
      (dolist (bufname '("*helm recentf*"
                         "*helm projectile*"
                         "*helm imenu*"
                         "*helm company*"
                         "*helm buffers*"
                         ;; "*helm tags*"
                         "*helm-ag*"
                         "*Helm Swoop*"))
        (push bufname winner-boring-buffers)))

    (after "company"
      (use-package helm-company
        :config
        (defun helm-company ()
          (interactive)
          (unless company-candidates
            (company-complete))
          (when company-point
            (helm :sources 'helm-source-company
                  :buffer  "*helm company*"
                  :prompt helm-global-prompt
                  :candidate-number-limit helm-company-candidate-number-limit)))))

    (after "projectile"
      (use-package helm-projectile
        :config
        (setq projectile-switch-project-action 'helm-projectile))

      ;; Don't show the project name in the prompts; I already know.
      (defun projectile-prepend-project-name (string)
        (format helm-global-prompt string))

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
                   :prompt helm-global-prompt))))

      (helm-projectile-command "switch-project" 'helm-source-projectile-projects helm-global-prompt)
      (helm-projectile-command "find-file" helm-source-projectile-files-and-dired-list helm-global-prompt)
      (helm-projectile-command "find-file-in-known-projects" 'helm-source-projectile-files-in-all-projects-list helm-global-prompt)
      (helm-projectile-command "find-file-dwim" 'helm-source-projectile-files-dwim-list helm-global-prompt)
      (helm-projectile-command "find-dir" helm-source-projectile-directories-and-dired-list helm-global-prompt)
      (helm-projectile-command "recentf" 'helm-source-projectile-recentf-list helm-global-prompt)
      (helm-projectile-command "switch-to-buffer" 'helm-source-projectile-buffers-list helm-global-prompt))

    (progn ; helm hacks
      ;; No persistent header
      (defadvice helm-display-mode-line (after undisplay-header activate)
        (setq header-line-format nil))

      ;; Reconfigured `helm-recentf' to use `helm', instead of `helm-other-buffer'
      (defun helm-recentf ()
        (interactive)
        (let ((helm-ff-transformer-show-only-basename nil))
          (helm :sources '(helm-source-recentf)
                :buffer "*helm recentf*"
                :prompt helm-global-prompt)))

      ;; Hide the mode-line in helm (<3 minimalism)
      (defun helm-display-mode-line (source &optional force)
        (set (make-local-variable 'helm-mode-line-string)
             (helm-interpret-value (or (and (listp source) ; Check if source is empty.
                                            (assoc-default 'mode-line source))
                                       (default-value 'helm-mode-line-string))
                                   source))
        (let ((follow (and (eq (cdr (assq 'follow source)) 1) "(HF) ")))
          (if helm-mode-line-string
              (setq mode-line-format nil)
            (setq mode-line-format (default-value 'mode-line-format)))
          (let* ((hlstr (helm-interpret-value
                         (and (listp source)
                              (assoc-default 'header-line source))
                         source))
                 (hlend (make-string (max 0 (- (window-width) (length hlstr))) ? )))
            (setq header-line-format
                  (propertize (concat " " hlstr hlend) 'face 'helm-header))))
        (when force (force-mode-line-update))))

    (progn ; popwin
      (push '("^\\*helm.*\\*$" :position bottom :regexp t :height 18)
            popwin:special-display-config))

    (progn ; evil
      (evil-set-initial-state 'helm-mode 'emacs)

      ;; Ex-mode interface for `helm-recentf' and `helm-projectile-recentf'. If
      ;; `bang', then `search' is interpreted as regexp
      (evil-ex-define-cmd "rec[ent]" 'my:helm-recentf)
      (evil-define-command my:helm-recentf (&optional bang)
        :repeat nil
        (interactive "<!>")
        (if bang (helm-recentf) (helm-projectile-recentf))))

    (bind helm-map
          "C-w"        'evil-delete-backward-word
          "C-u"        'helm-delete-minibuffer-contents
          "C-r"        'evil-ex-paste-from-register ; Evil registers in helm! Glorious!
          [escape]     'helm-keyboard-quit)))


(provide 'init-helm)
;;; init-helm.el ends here
