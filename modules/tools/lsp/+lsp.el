;;; tools/lsp/+lsp.el -*- lexical-binding: t; -*-

(use-package! lsp-mode
  :commands lsp-install-server
  :init
  (setq lsp-session-file (concat doom-etc-dir "lsp-session"))
  ;; Auto-kill LSP server after last workspace buffer is killed.
  (setq lsp-keep-workspace-alive nil)
  ;; Let `flycheck-check-syntax-automatically' determine this. Will be removed
  ;; soon: https://github.com/emacs-lsp/lsp-mode/pull/1701/files
  (setq lsp-flycheck-live-reporting nil)
  ;; For `lsp-clients'
  (setq lsp-server-install-dir (concat doom-etc-dir "lsp/")
        lsp-intelephense-storage-path (concat doom-cache-dir "lsp-intelephense/"))

  (when (featurep! :config default +bindings)
    ;; Let doom bind the lsp keymap.
    (setq lsp-keymap-prefix nil))

  ;; Disable LSP's superfluous, expensive and/or debatably unnecessary features.
  ;; Some servers implement these poorly. Better to just rely on Emacs' native
  ;; mechanisms and make these opt-in.
  (setq lsp-enable-folding nil
        ;; HACK Fix #2911, until it is resolved upstream. Links come in
        ;;      asynchronously from the server, but lsp makes no effort to
        ;;      "select" the original buffer before laying them down, so they
        ;;      could be rendered in the wrong buffer (like the minibuffer).
        lsp-enable-links nil
        ;; Potentially slow
        lsp-enable-file-watchers nil
        lsp-enable-text-document-color nil
        lsp-enable-semantic-highlighting nil
        ;; Don't modify our code without our permission
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        ;; capf is the preferred completion mechanism for lsp-mode now
        lsp-prefer-capf t)

  :config
  (set-popup-rule! "^\\*lsp-help" :size 0.35 :quit t :select t)
  (set-lookup-handlers! 'lsp-mode :async t
    :documentation #'lsp-describe-thing-at-point
    :definition #'lsp-find-definition
    :implementations #'lsp-find-implementation
    :type-definition #'lsp-find-type-definition
    :references #'lsp-find-references)

  ;; TODO Lazy load these. They don't need to be loaded all at once unless the
  ;;      user uses `lsp-install-server'.
  (when lsp-auto-configure
    (mapc (lambda (package) (require package nil t))
          lsp-client-packages))

  (defadvice! +lsp-init-a (&optional arg)
    "Enable `lsp-mode' in the current buffer.

Meant to gimp `lsp', which is too eager about installing LSP servers, or
prompting to do so, or complaining about no LSP servers, or initializing
lsp-ui-mode, company, yasnippet and flycheck. We want LSP to work only if the
server is present, and for server installation to be a deliberate act by the
end-user. Also, setting up these other packages are handled by their respective
modules.

Also see:
+ `+lsp-init-company-h' (on `lsp-mode-hook')
+ `+lsp-init-flycheck-or-flymake-h' (on `lsp-mode-hook')

This also logs the resolved project root, if found, so we know where we are."
    :override #'lsp
    (interactive "P")
    (and (buffer-file-name (buffer-base-buffer))
         (require 'lsp-mode nil t)
         (setq-local
          lsp--buffer-workspaces
          (or (lsp--try-open-in-library-workspace)
              (lsp--try-project-root-workspaces
               (equal arg '(4))
               (and arg (not (equal arg 1))))))
         ;; read-process-output-max is only available on recent
         ;; development builds of Emacs 27 and above
         (or (not (boundp 'read-process-output-max))
             (setq-local read-process-output-max (* 1024 1024)))
         ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
         ;;        native JSON library, so we up the GC threshold to stave off
         ;;        GC-induced slowdowns/freezes.
         (setq-local gcmh-high-cons-threshold (* 2 gcmh-high-cons-threshold))
         (prog1 (lsp-mode 1)
           (setq-local lsp-buffer-uri (lsp--buffer-uri))
           ;; Announce what project root we're using, for diagnostic purposes
           (if-let (root (lsp--calculate-root (lsp-session) (buffer-file-name)))
               (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
             (lsp--info "Could not guess project root."))
           (lsp--info "Connected to %s."
                      (apply #'concat
                             (mapcar
                              (lambda (it) (format "[%s]" (lsp--workspace-print it)))
                              lsp--buffer-workspaces))))))

  (when (featurep! :config default +bindings)
    (dolist (leader-key (list doom-leader-key doom-leader-alt-key))
      (let ((lsp-keymap-prefix (concat leader-key " c l")))
        (lsp-enable-which-key-integration))))

  (add-hook! 'lsp-mode-hook
    (defun +lsp-init-company-h ()
      (if (not (bound-and-true-p company-mode))
          (add-hook 'company-mode-hook #'+lsp-init-company-h t t)
        ;; Ensure `company-capf' is at the front of `company-backends'
        (setq-local company-backends
                    (cons 'company-capf
                          (remq 'company-capf company-backends)))
        (remove-hook 'company-mode-hook #'+lsp-init-company-h t)))
    (defun +lsp-init-flycheck-or-flymake-h ()
      "Set up flycheck-mode or flymake-mode, depending on `lsp-diagnostic-package'."
      (pcase lsp-diagnostic-package
        ((or :auto 'nil)                ; try flycheck, fall back to flymake
         (let ((lsp-diagnostic-package
                (if (require 'flycheck nil t) :flycheck :flymake)))
           (+lsp-init-flycheck-or-flymake-h)))
        ((or :flymake 't)
         (lsp--flymake-setup))
        (:flycheck
         ;; Ensure file/dir local `flycheck-checker' is respected
         (unless flycheck-checker
           (if (flycheck-checker-supports-major-mode-p 'lsp major-mode)
               (lsp-flycheck-enable)
             (let ((old-checker (flycheck-get-checker-for-buffer)))
               (lsp-flycheck-enable)
               (when old-checker
                 (flycheck-add-next-checker 'lsp old-checker))))
           (kill-local-variable 'flycheck-checker))))))

  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process)."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null +lsp-defer-shutdown)
            (= +lsp-defer-shutdown 0))
        (funcall orig-fn restart)
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
             nil (lambda (workspace)
                   (let ((lsp--cur-workspace workspace))
                     (unless (lsp--workspace-buffers lsp--cur-workspace)
                       (funcall orig-fn))))
             lsp--cur-workspace))))

  ;; Don't prompt to restart LSP servers while quitting Emacs
  (add-hook! 'kill-emacs-hook (setq lsp-restart 'ignore)))


(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-doc is redundant with and more invasive than
        ;; `+lookup/documentation'
        lsp-ui-doc-enable nil
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil)

  (when (featurep! +peek)
    (set-lookup-handlers! 'lsp-ui-mode :async t
      :definition 'lsp-ui-peek-find-definitions
      :implementations 'lsp-ui-peek-find-implementation
      :references 'lsp-ui-peek-find-references)))


(use-package! helm-lsp
  :when (featurep! :completion helm)
  :commands helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol)


(use-package! lsp-ivy
  :when (featurep! :completion ivy)
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
