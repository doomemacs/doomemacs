;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(use-package! envrc
  :hook (doom-first-file . envrc-global-mode)
  :config
  (set-debug-var! 'envrc-debug)

  (set-popup-rule! "^\\*envrc\\*" :quit t :ttl 0)

  ;; HACK: Normally, envrc updates on `after-change-major-mode-hook' (runs after
  ;;   a major mode's body and hooks). IMHO, this is too late; a mode's hooks
  ;;   might depend on environmental state that direnv sets up (e.g. starting an
  ;;   LSP server that expects project-specific envvars), so I move it to
  ;;   `change-major-mode-after-body-hook' instead, which runs before said
  ;;   hooks, but not the body.
  (add-hook! 'envrc-global-mode-hook
    (defun +direnv-init-global-mode-earlier-h ()
      (let ((fn (if (fboundp #'envrc-global-mode-enable-in-buffers)
                    #'envrc-global-mode-enable-in-buffers ; Removed in Emacs 30.
                  #'envrc-global-mode-enable-in-buffer)))
        (if (not envrc-global-mode)
            (remove-hook 'change-major-mode-after-body-hook fn)
          (remove-hook 'after-change-major-mode-hook fn)
          (add-hook 'change-major-mode-after-body-hook fn 100)))))

  ;; HACK: Reloading direnv doesn't restart the associated LSP/eglot clients, so
  ;;   this restarts them for you.
  (when (modulep! :tools lsp)
    (defadvice! +direnv--restart-lsp-servers-a (env-dir)
      :after #'envrc--update-env
      (let (eglot-servers lsp-servers)
        (dolist (buf (envrc--mode-buffers))
          (with-current-buffer buf
            (when (string= (envrc--find-env-dir) env-dir)
              (when (bound-and-true-p lsp-mode)
                (dolist (ws (lsp-workspaces))
                  (cl-pushnew ws lsp-servers :test #'equal)))
              (when (bound-and-true-p eglot--managed-mode)
                (cl-pushnew (eglot-current-server) eglot-servers :test #'equal)))))
        (when (or eglot-servers lsp-servers)
          (mapc #'eglot-reconnect eglot-servers)
          (mapc #'lsp-workspace-restart lsp-servers)
          (message "Restarted %d lsp/eglot servers associated with direnv"
                   (+ (length eglot-servers)
                      (length lsp-servers)))))))

  ;; ...However, the above hack causes envrc to trigger in its own, internal
  ;; buffers, causing extra direnv errors.
  (defadvice! +direnv--debounce-update-a (&rest _)
    "Prevent direnv from running multiple times, consecutively in a buffer."
    :before-while #'envrc--update
    (not (string-prefix-p "*envrc" (buffer-name))))

  (defadvice! +direnv--fail-gracefully-a (&rest _)
    "Don't try to use direnv if the executable isn't present."
    :before-while #'envrc-global-mode
    (or (executable-find envrc-direnv-executable)
        (ignore (doom-log "Failed to locate direnv executable; aborting envrc-global-mode"))))

  ;; Ensure babel's execution environment matches the host buffer's.
  (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment)

  ;; Make sure any envrc changes are propagated after a `doom/reload'
  (add-hook 'doom-after-reload-hook #'envrc-reload-all))
