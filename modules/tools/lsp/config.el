;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(defvar +lsp-company-backend 'company-lsp
  "What backend to prepend to `company-backends' when `lsp-mode' is active.

This can be a single company backend or a list thereof. It can be anything
`company-backends' will accept.")


(setq lsp-session-file (concat doom-etc-dir "lsp-session")
      lsp-auto-guess-root t
      lsp-keep-workspace-alive nil
      lsp-groovy-server-install-dir (concat doom-etc-dir "lsp-groovy/")
      lsp-intelephense-storage-path (concat doom-cache-dir "lsp-intelephense/"))


(after! lsp-mode
  (set-lookup-handlers! 'lsp-mode :async t
    :documentation 'lsp-describe-thing-at-point
    :definition 'lsp-find-definition
    :references 'lsp-find-references)

  ;; The original `lsp' initializes too much, too quickly. Things like flycheck,
  ;; company, and yasnippet. Doom's modules already handle these just fine, so
  ;; leave it to us.
  (advice-add #'lsp :override #'lsp!)

  ;; Don't prompt to restart LSP servers while quitting Emacs
  (add-hook! 'kill-emacs-hook (setq lsp-restart 'ignore)))


(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (add-hook! 'lsp-ui-mode-hook
    (defun +lsp-init-ui-flycheck-or-flymake-h ()
      "Sets up flymake-mode or flycheck-mode, depending on `lsp-prefer-flymake'."
      (cond ((eq :none lsp-prefer-flymake))
            ((and (not (version< emacs-version "26.1"))
                  lsp-prefer-flymake)
             (lsp--flymake-setup))
            ((require 'flycheck nil t)
             (require 'lsp-ui-flycheck)
             (lsp-ui-flycheck-enable t)))))
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
