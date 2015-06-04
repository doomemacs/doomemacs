(use-package helm
  :commands (helm
             helm-etags-select
             helm-show-kill-ring
             helm-bookmarks
             helm-wg
             helm-ag
             helm-alive-p
             helm-attrset)
  :init
  (progn
    (defvar helm-global-prompt ">>> ")
    (evil-set-initial-state 'helm-mode 'emacs))
  :config
  (progn
    (use-package helm-grep)
    (use-package helm-ag
      :functions (helm-ag--select-source)
      :defines (helm-ag-source helm-ag--last-query))

    ;; No persistent header
    (defun narf--helm-display-mode-line (source &optional force)
      (setq mode-line-format nil)
      (setq header-line-format nil))
    (advice-add 'helm-display-mode-line :override 'narf--helm-display-mode-line)

    ;; Minimalistic split-fn; leaves popwin to handle helm buffers
    (defun narf--helm-split-window-fn (window)
      (if (one-window-p t)
          (let ((helm-full-frame t))
            (selected-window))
        (other-window-for-scrolling)))

    (setq helm-quick-update t
          helm-idle-delay 0.05
          helm-input-idle-delay 0.05
          helm-reuse-last-window-split-state t
          helm-buffers-fuzzy-matching t
          helm-candidate-number-limit 40
          helm-bookmark-show-location t
          helm-split-window-default-side 'other
          helm-split-window-preferred-function 'narf--helm-split-window-fn)   ; let popwin handle this

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
    (narf/add-throwaway-buffer "^\\*[Hh]elm.*\\*$")

    (bind :map helm-map
          "C-w"        'evil-delete-backward-word
          "C-u"        'helm-delete-minibuffer-contents
          "C-r"        'evil-ex-paste-from-register ; Evil registers in helm! Glorious!
          [escape]     'helm-keyboard-quit)))

(use-package projectile
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-grep
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-find-test-file
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc
             projectile-project-p
             projectile-global-mode
             helm-projectile-switch-to-buffer
             helm-projectile-find-file
             helm-projectile-recentf
             helm-projectile-find-other-file
             helm-projectile-switch-project)
  :diminish projectile-mode
  :config
  (progn
    (defun narf/project-invalidate-cache-maybe ()
      (when (projectile-project-p) (projectile-invalidate-cache nil)))
    (add-hook 'kill-emacs-hook 'narf/project-invalidate-cache-maybe)
    (setq-default projectile-enable-caching t)
    (setq projectile-sort-order 'recentf
          projectile-cache-file (concat TMP-DIR "projectile.cache")
          projectile-known-projects-file (concat TMP-DIR "projectile.projects")
          projectile-indexing-method 'alien
          projectile-project-root-files narf/project-root-files)
    (add-to-list 'projectile-globally-ignored-files "ido.last")
    (add-to-list 'projectile-globally-ignored-directories "assets")
    (add-to-list 'projectile-other-file-alist '("scss" "css"))
    (add-to-list 'projectile-other-file-alist '("css" "scss"))

    (use-package helm-projectile)
    (projectile-global-mode +1)

    ;; Don't show the project name in the prompts; I already know.
    (defun projectile-prepend-project-name (string) helm-global-prompt)))

(use-package helm-org
  :commands (helm-org-in-buffer-headings
             helm-org-agenda-files-headings
             helm-org-capture-templates)
  :config
  (defun helm-get-org-candidates-in-file (filename min-depth max-depth
                                                   &optional fontify nofname)
    (with-current-buffer (pcase filename
                           ((pred bufferp) filename)
                           ((pred stringp) (find-file-noselect filename)))
      (and fontify (jit-lock-fontify-now))
      (let ((match-fn (if fontify 'match-string 'match-string-no-properties)))
        (save-excursion
          (goto-char (point-min))
          (cl-loop with width = (window-width)
                   while (re-search-forward org-complex-heading-regexp nil t)
                   if (let ((num-stars (length (match-string-no-properties 1))))
                        (and (>= num-stars min-depth) (<= num-stars max-depth)))
                   collect `(,(let ((heading (funcall match-fn 4))
                                    (file (unless nofname
                                            (concat (f-no-ext (f-relative filename org-directory)) ":")))
                                    (level (length (match-string-no-properties 1))))
                                (org-format-outline-path
                                 (append (org-get-outline-path t level heading)
                                         (list heading)) width file))
                             . ,(point-marker))))))))

(use-package helm-files
  :commands helm-recentf
  :config
  (progn
    ;; Reconfigured `helm-recentf' to use `helm', instead of `helm-other-buffer'
    (defun helm-recentf ()
      (interactive)
      (let ((helm-ff-transformer-show-only-basename nil))
        (helm :sources '(helm-source-recentf)
              :buffer "*helm recentf*"
              :prompt helm-global-prompt)))))

(use-package helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment))

(use-package helm-swoop    ; https://github.com/ShingoFukuyama/helm-swoop
  :defines (helm-swoop-last-prefix-number)
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-split-with-multiple-windows t
        helm-swoop-speed-or-color t))

(use-package helm-c-yasnippet :commands helm-yas-visit-snippet-file)
(use-package helm-buffers     :commands helm-buffers-list)
(use-package helm-semantic    :commands helm-semantic-or-imenu)
(use-package helm-elisp       :commands helm-apropos)
(use-package helm-command     :commands helm-M-x)
(use-package helm-company     :defer t)


(provide 'init-helm)
;;; init-helm.el ends here
