;;; core-helm.el

(use-package helm
  :commands (helm
             helm-etags-select
             helm-show-kill-ring
             helm-bookmarks
             helm-alive-p
             helm-attrset)
  :init
  (defvar helm-global-prompt ">>> ")
  (setq helm-quick-update t
        helm-idle-delay 0.05
        helm-input-idle-delay 0.05
        helm-reuse-last-window-split-state t
        helm-buffers-fuzzy-matching t
        helm-candidate-number-limit 40
        helm-bookmark-show-location t
        ;; let popwin handle this
        helm-split-window-default-side 'other
        helm-split-window-preferred-function 'narf/helm-split-window)
  :config
  (evil-set-initial-state 'helm-mode 'emacs)

  (after! winner
    (dolist (bufname '("*helm recentf*"
                       "*helm projectile*"
                       "*helm imenu*"
                       "*helm company*"
                       "*helm buffers*"
                       "*helm "
                       "*Helm Css SCSS*"
                       "*helm-ag*"
                       "*helm-ag-edit*"
                       "*Helm Swoop*"))
      (push bufname winner-boring-buffers)))

  (bind! :map helm-map
         "<backtab>"  'helm-ag-edit
         "C-w"        'evil-delete-backward-word
         "C-u"        'helm-delete-minibuffer-contents
         "C-r"        'evil-ex-paste-from-register ; Evil registers in helm! Glorious!
         [escape]     'helm-keyboard-quit)

  ;; Hide mode-line in helm windows
  (advice-add 'helm-display-mode-line :override 'narf*helm-hide-modeline))

(use-package helm-ag
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
  (defadvice helm-ag--edit-abort (around helm-ag-edit-abort-popwin-compat activate)
    (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it))
  (defadvice helm-ag--edit-commit (around helm-ag-edit-commit-popwin-compat activate)
    (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it))

  ;; I remove any attempt to kill the helm-ag window, because popwin handles it.
  (defun helm-ag--edit (_candidate)
    (let ((default-directory helm-ag--default-directory))
      (with-current-buffer (get-buffer-create "*helm-ag-edit*")
        (erase-buffer)
        (setq-local helm-ag--default-directory helm-ag--default-directory)
        (let (buf-content)
          (with-current-buffer (get-buffer "*helm-ag*")
            (goto-char (point-min))
            (forward-line 1)
            (let* ((body-start (point))
                   (marked-lines (cl-loop for ov in (overlays-in body-start (point-max))
                                          when (eq 'helm-visible-mark (overlay-get ov 'face))
                                          return (helm-marked-candidates))))
              (if (not marked-lines)
                  (setq buf-content (buffer-substring-no-properties
                                     body-start (point-max)))
                (setq buf-content (concat (mapconcat 'identity marked-lines "\n") "\n")))))
          (insert buf-content)
          (add-text-properties (point-min) (point-max)
                               '(read-only t rear-nonsticky t front-sticky t))
          (let ((inhibit-read-only t))
            (setq header-line-format
                  (format "[%s] C-c C-c: Commit, C-c C-k: Abort"
                          (abbreviate-file-name helm-ag--default-directory)))
            (goto-char (point-min))
            (while (re-search-forward "^\\(\\(?:[^:]+:\\)\\{1,2\\}\\)\\(.*\\)$" nil t)
              (let ((file-line-begin (match-beginning 1))
                    (file-line-end (match-end 1))
                    (body-begin (match-beginning 2))
                    (body-end (match-end 2)))
                (add-text-properties file-line-begin file-line-end
                                     '(face font-lock-function-name-face
                                            intangible t))
                (remove-text-properties body-begin body-end '(read-only t))
                (set-text-properties body-end (1+ body-end)
                                     '(read-only t rear-nonsticky t))))))))
    (popwin:display-buffer (get-buffer "*helm-ag-edit*"))
    ;; (other-window 1)
    ;; (switch-to-buffer (get-buffer "*helm-ag-edit*"))
    (goto-char (point-min))
    (setq next-error-function 'compilation-next-error-function)
    (setq-local compilation-locs (make-hash-table :test 'equal :weakness 'value))
    (use-local-map helm-ag-edit-map)))

(use-package helm-org
  :commands (helm-org-in-buffer-headings
             helm-org-agenda-files-headings
             helm-org-capture-templates))

(use-package helm-files
  :commands (helm-recentf
             helm-buffers
             helm-buffers-list)
  :config
  (defun helm-recentf ()
    "Reconfigured `helm-recentf' to use `helm', instead of `helm-other-buffer'"
    (interactive)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources '(helm-source-recentf)
            :buffer "*helm recentf*"
            :prompt helm-global-prompt))))

(use-package helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment)
  :config
  (setq helm-css-scss-split-direction 'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))

(use-package helm-swoop    ; https://github.com/ShingoFukuyama/helm-swoop
  :defines  (helm-swoop-last-prefix-number)
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-split-with-multiple-windows t
        helm-swoop-speed-or-color t))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-ack projectile-ag projectile-compile-project projectile-dired
             projectile-grep projectile-find-dir projectile-find-file projectile-find-tag
             projectile-find-test-file projectile-invalidate-cache projectile-kill-buffers
             projectile-multi-occur projectile-project-root projectile-recentf
             projectile-regenerate-tags projectile-replace
             projectile-run-async-shell-command-in-root projectile-run-shell-command-in-root
             projectile-switch-project projectile-switch-to-buffer projectile-vc
             projectile-project-p projectile-global-mode)
  :config
  (add-hook! kill-emacs 'narf|projectile-invalidate-cache-maybe)

  (setq-default projectile-enable-caching t)
  (setq projectile-sort-order 'recentf
        projectile-cache-file (! (concat narf-temp-dir "projectile.cache"))
        projectile-known-projects-file (! (concat narf-temp-dir "projectile.projects"))
        projectile-indexing-method 'alien
        projectile-project-root-files narf-project-root-files)

  (add-to-list 'projectile-globally-ignored-files "ido.last")
  (add-to-list 'projectile-globally-ignored-directories "assets")
  (add-to-list 'projectile-other-file-alist '("scss" "css"))
  (add-to-list 'projectile-other-file-alist '("css" "scss"))

  (projectile-global-mode +1)

  (advice-add 'projectile-prepend-project-name :override 'narf*projectile-replace-prompt))

(use-package helm-projectile
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-file
             helm-projectile-recentf
             helm-projectile-find-other-file
             helm-projectile-switch-project))

;; (use-package helm-c-yasnippet :commands helm-yas-visit-snippet-file)
(use-package helm-semantic :commands helm-semantic-or-imenu)
(use-package helm-elisp    :commands helm-apropos)
(use-package helm-command  :commands helm-M-x)

(provide 'core-helm)
;;; core-helm.el ends here
