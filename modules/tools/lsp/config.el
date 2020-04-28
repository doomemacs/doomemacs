;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(defvar +lsp-company-backend 'company-lsp
  "What backend to use for lsp-driven autocompletion.

This can be overridden by `+lsp-capf-blacklist'.

While `company-capf' does not require the `company-lsp' package and should offer
better performance, it has been integrated into lsp only recently and as of
02/25/2020 is known to cause issues with some language servers. If you wish to
use `company-capf' in general but fall back to `company-lsp' for specific
language servers, set `+lsp-company-backend' to `company-capf' and add the
excluded servers' identifiers to `+lsp-capf-blacklist'.")

(defvar +lsp-capf-blacklist '(ts-ls gopls)
  "Language servers listed here will always use the `company-lsp' backend,
irrespective of what `+lsp-company-backend' is set to.")

(defvar +lsp-defer-shutdown 3
  "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer.")


;;
;;; Packages

(use-package! lsp-mode
  :commands lsp-install-server
  :init
  (setq lsp-session-file (concat doom-etc-dir "lsp-session"))
  ;; Don't prompt the user for the project root every time we open a new
  ;; lsp-worthy file, instead, try to guess it with projectile.
  (setq lsp-auto-guess-root t)
  ;; Auto-kill LSP server after last workspace buffer is killed.
  (setq lsp-keep-workspace-alive nil)
  ;; Let `flycheck-check-syntax-automatically' determine this.
  (setq lsp-flycheck-live-reporting nil)
  ;; For `lsp-clients'
  (setq lsp-server-install-dir (concat doom-etc-dir "lsp/")
        lsp-groovy-server-install-dir (concat lsp-server-install-dir "lsp-groovy/")
        lsp-intelephense-storage-path (concat doom-cache-dir "lsp-intelephense/"))

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
        lsp-enable-on-type-formatting nil)

  :config
  (set-popup-rule! "^\\*lsp-help" :size 0.35 :quit t :select t)
  (set-lookup-handlers! 'lsp-mode :async t
    :documentation #'lsp-describe-thing-at-point
    :definition #'lsp-find-definition
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

  (add-hook! 'lsp-mode-hook
    (defun +lsp-init-company-h ()
      (if (not (bound-and-true-p company-mode))
          (add-hook 'company-mode-hook #'+lsp-init-company-h t t)
        (let ((preferred-backend +lsp-company-backend))
          (lsp-foreach-workspace
           (when (memq (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace))
                       +lsp-capf-blacklist)
             (setq preferred-backend 'company-lsp)))
          (if (eq 'company-capf preferred-backend)
              ;; use capf backend
              (progn
                (setq-local lsp-enable-completion-at-point t)
                (setq-local lsp-prefer-capf t)
                (setq-local company-backends
                            (cons 'company-capf (remq 'company-capf company-backends))))
            ;; use company-lsp backend (may need to be loaded first)
            (require 'company-lsp)
            (setq-local lsp-enable-completion-at-point nil)
            (setq-local lsp-prefer-capf nil)
            (setq-local company-backends
                        (cons 'company-lsp (remq 'company-capf company-backends)))
            (setq-default company-lsp-cache-candidates 'auto))
          (remove-hook 'company-mode-hook #'+lsp-init-company-h t))))
    (defun +lsp-init-flycheck-or-flymake-h ()
      "Set up flycheck-mode or flymake-mode, depending on `lsp-diagnostic-package'."
      (pcase lsp-diagnostic-package
        ((or :auto 'nil) ; try flycheck, fall back to flymake
         (let ((lsp-diagnostic-package
                (if (require 'flycheck nil t) :flycheck :flymake)))
           (+lsp-init-flycheck-or-flymake-h)))
        ((or :flymake 't)
         (lsp--flymake-setup))
        (:flycheck
         (let ((old-checker flycheck-checker))
           (lsp-flycheck-enable)
           ;; Ensure file/dir local `flycheck-checker' is respected
           (when old-checker
             (setq-local flycheck-checker old-checker)
             (kill-local-variable 'flycheck-check-syntax-automatically)))))))

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

  (defadvice! +lsp-prompt-if-no-project-a (session file-name)
    "Prompt for the project root only if no project was found."
    :after-until #'lsp--calculate-root
    (cond ((not lsp-auto-guess-root)
           nil)
          ((cl-find-if (lambda (dir)
                         (and (lsp--files-same-host dir file-name)
                              (file-in-directory-p file-name dir)))
                       (lsp-session-folders-blacklist session))
           nil)
          ((lsp--find-root-interactively session))))

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
      :references 'lsp-ui-peek-find-references)))


(use-package! company-lsp
  :defer t
  :config
  (setq company-lsp-cache-candidates 'auto)
  ;; HACK Fix tigersoldier/company-lsp#128 causing company-lsp results to
  ;;      display candidates that are unrelated to the prefix. Source:
  ;;      emacs-lsp/lsp-python-ms#79
  (add-to-list 'company-lsp-filter-candidates '(mspyls . t))
  (defadvice! +company---fix-lsp-caching-on-competion-a (response prefix)
    :override #'company-lsp--on-completion
    (let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
           (items (cond ((hash-table-p response) (gethash "items" response))
                        ((sequencep response) response)))
           (candidates (mapcar (lambda (item)
                                 (company-lsp--make-candidate item prefix))
                               (lsp--sort-completions items)))
           (server-id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace)))
           (should-filter (or
                           ;; CHANGE BEGIN
                           (eq company-lsp-cache-candidates t)
                           ;; CHANGE END
                           (and (null company-lsp-cache-candidates)
                                (company-lsp--get-config company-lsp-filter-candidates server-id)))))
      (when (null company-lsp--completion-cache)
        (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache nil t)
        (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache nil t))
      (when (eq company-lsp-cache-candidates 'auto)
        (company-lsp--cache-put prefix (company-lsp--cache-item-new candidates incomplete)))
      (if should-filter
          (company-lsp--filter-candidates candidates prefix)
        candidates))))


(use-package! helm-lsp
  :when (featurep! :completion helm)
  :commands helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol)


(use-package! lsp-ivy
  :when (featurep! :completion ivy)
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
