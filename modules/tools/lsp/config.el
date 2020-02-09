;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(defvar +lsp-company-backend 'company-lsp
  "What backend to prepend to `company-backends' when `lsp-mode' is active.

This can be a single company backend or a list thereof. It can be anything
`company-backends' will accept.")


;;
;;; Packages

(use-package! lsp-mode
  :commands lsp-install-server
  :init
  (setq lsp-session-file (concat doom-etc-dir "lsp-session"))
  ;; Don't prompt the user for the project root every time we open a new
  ;; lsp-worthy file, instead, try to guess it with projectile.
  (setq lsp-auto-guess-root t)
  ;; Auto-kill LSP server once you've killed the last buffer associated with its
  ;; project.
  (setq lsp-keep-workspace-alive nil)

  ;; For `lsp-clients'
  (setq lsp-server-install-dir (concat doom-etc-dir "lsp/")
        lsp-groovy-server-install-dir (concat lsp-server-install-dir "lsp-groovy/")
        lsp-intelephense-storage-path (concat doom-cache-dir "lsp-intelephense/"))

  :config
  (when (and lsp-auto-configure lsp-auto-require-clients)
    (require 'lsp-clients))

  (set-lookup-handlers! 'lsp-mode :async t
    :documentation 'lsp-describe-thing-at-point
    :definition 'lsp-find-definition
    :references 'lsp-find-references)

  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is usually an expensive process)."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart)
        (funcall orig-fn)
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             3 nil (lambda (workspace)
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
+ `+lsp-init-ui-flycheck-or-flymake-h' (on `lsp-ui-mode-hook')

This also logs the resolved project root, if found, so we know where we are."
    :override #'lsp
    (interactive "P")
    (and (buffer-file-name)
         (require 'lsp-mode nil t)
         (setq-local
          lsp--buffer-workspaces
          (or (lsp--try-open-in-library-workspace)
              (lsp--try-project-root-workspaces
               (equal arg '(4))
               (and arg (not (equal arg 1))))))
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

  ;; Don't prompt to restart LSP servers while quitting Emacs
  (add-hook! 'kill-emacs-hook (setq lsp-restart 'ignore)))


(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (add-hook! 'lsp-ui-mode-hook
    (defun +lsp-init-ui-flycheck-or-flymake-h ()
      "Sets up flymake-mode or flycheck-mode, depending on `lsp-prefer-flymake'."
      (cond ((eq :none lsp-prefer-flymake))
            (lsp-prefer-flymake
             (lsp--flymake-setup))
            ((require 'flycheck nil t)
             (require 'lsp-ui-flycheck)
             (let ((old-checker flycheck-checker))
               (lsp-ui-flycheck-enable t)
               (when old-checker
                 (setq-local flycheck-checker old-checker)
                 (kill-local-variable 'flycheck-check-syntax-automatically)))))))

  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-doc is redundant with and more invasive than
        ;; `+lookup/documentation'
        lsp-ui-doc-enable nil
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil)

  (set-lookup-handlers! 'lsp-ui-mode :async t
    :definition 'lsp-ui-peek-find-definitions
    :references 'lsp-ui-peek-find-references))


(use-package! company-lsp
  :when (featurep! :completion company)
  :defer t
  :init
  ;; Make sure that `company-capf' is disabled since it is incompatible with
  ;; `company-lsp' (see lsp-mode#884)
  (add-hook! 'lsp-mode-hook
    (defun +lsp-init-company-h ()
      (if (not (bound-and-true-p company-mode))
          (add-hook 'company-mode-hook #'+lsp-init-company-h t t)
        (setq-local company-backends
                    (cons +lsp-company-backend
                          (remq 'company-capf company-backends)))
        (remove-hook 'company-mode-hook #'+lsp-init-company-h t))))
  :config
  (setq company-lsp-cache-candidates 'auto)) ;; cache candidates for better performance


(use-package! helm-lsp
  :when (featurep! :completion helm)
  :commands helm-lsp-workspace-symbol helm-lsp-global-workspace-symbol)


(use-package! lsp-ivy
  :when (featurep! :completion ivy)
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
