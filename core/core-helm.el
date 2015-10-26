;;; core-helm.el

(use-package helm
  :init
  (defvar helm-global-prompt ">>> ")
  (setq-default
   helm-quick-update t
   helm-reuse-last-window-split-state t

   helm-buffers-fuzzy-matching t
   helm-apropos-fuzzy-match t
   helm-M-x-fuzzy-match t
   helm-recentf-fuzzy-match t
   ;; helm-mode-fuzzy-match t

   helm-ff-auto-update-initial-value nil
   helm-find-files-doc-header nil

   helm-candidate-number-limit 30
   helm-bookmark-show-location t
   ;; let popwin handle this
   helm-split-window-default-side 'other
   helm-split-window-preferred-function 'narf/helm-split-window)
  :config
  (evil-set-initial-state 'helm-mode 'emacs)

  ;; Rewrite prompt for all helm windows
  (defun helm (&rest plist)
    (let ((fn (cond ((or (and helm-alive-p (plist-get plist :allow-nest))
                         (and helm-alive-p (memq 'allow-nest plist)))
                     #'helm-nest)
                    ((keywordp (car plist))
                     #'helm)
                    (t #'helm-internal))))
      (if (and helm-alive-p (eq fn #'helm))
          (if (helm-alive-p)
              (error "Error: Trying to run helm within a running helm session")
            (with-helm-buffer
              (prog1
                  (message "Aborting an helm session running in background")
                ;; `helm-alive-p' will be reset in unwind-protect forms.
                (helm-keyboard-quit))))
        (if (keywordp (car plist))
            (progn
              (setq helm--local-variables
                    (append helm--local-variables
                            (helm-parse-keys plist)))
              (apply fn (mapcar (lambda (key) (if (eq key :prompt) helm-global-prompt (plist-get plist key)))
                                helm-argument-keys)))
          (apply fn plist)))))

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

  (bind! (:map (helm-map helm-generic-files-map helm-find-files-map)
           "C-w"        'evil-delete-backward-word
           "C-r"        'evil-ex-paste-from-register ; Evil registers in helm! Glorious!
           [escape]     'helm-keyboard-quit)
         (:map helm-find-files-map
           "C-w"        'helm-find-files-up-one-level
           "TAB"        'helm-execute-persistent-action
           "/"          'helm-execute-persistent-action)
         (:map helm-ag-map
           "<backtab>"  'helm-ag-edit)
         (:map helm-ag-edit-map
           "<escape>"   'helm-ag--edit-abort
           :n "zx"      'helm-ag--edit-abort)
         (:map helm-map
           "C-u"        'helm-delete-minibuffer-contents))

  ;; Hide mode-line in helm windows
  (advice-add 'helm-display-mode-line :override 'narf*helm-hide-modeline))

(use-package projectile
  :diminish projectile-mode
  :config
  (add-hook! kill-emacs 'narf|projectile-invalidate-cache-maybe)

  (setq-default projectile-enable-caching t)
  (setq projectile-sort-order 'recentf
        projectile-require-project-root nil
        projectile-cache-file (concat narf-temp-dir "projectile.cache")
        projectile-known-projects-file (concat narf-temp-dir "projectile.projects")
        projectile-indexing-method 'alien
        projectile-project-root-files narf-project-root-files)

  (add-to-list 'projectile-globally-ignored-files "ido.last")
  (add-to-list 'projectile-globally-ignored-directories "assets")
  (add-to-list 'projectile-other-file-alist '("scss" "css"))
  (add-to-list 'projectile-other-file-alist '("css" "scss"))

  (projectile-global-mode +1)

  (advice-add 'projectile-prepend-project-name :override 'narf*projectile-replace-prompt)

  (require 'helm-projectile))

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
             helm-buffers-list))

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

;; (use-package helm-c-yasnippet :commands helm-yas-visit-snippet-file)
(use-package helm-semantic :commands helm-semantic-or-imenu)
(use-package helm-elisp    :commands helm-apropos)
(use-package helm-command  :commands helm-M-x)
(use-package helm-descbinds :command helm-descbinds)

(provide 'core-helm)
;;; core-helm.el ends here
