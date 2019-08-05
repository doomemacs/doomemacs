;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(setq lsp-session-file (concat doom-etc-dir "lsp-session")
      lsp-auto-guess-root t
      lsp-keep-workspace-alive nil
      lsp-groovy-server-install-dir (concat doom-etc-dir "groovy-langserver/"))

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


(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (defun +lsp|init-ui-flycheck-or-flymake ()
    "Sets up flymake-mode or flycheck-mode, depending on `lsp-prefer-flymake'."
    (unless (eq :none lsp-prefer-flymake)
      (if (and (not (version< emacs-version "26.1"))
               lsp-prefer-flymake)
          (lsp--flymake-setup))
      (require 'lsp-ui-flycheck)
      (lsp-ui-flycheck-enable t)))
  (add-hook 'lsp-ui-mode-hook #'+lsp|init-ui-flycheck-or-flymake)
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-doc is redundant with and less invasive than
        ;; `+lookup/documentation'
        lsp-ui-doc-enable nil)

  (set-lookup-handlers! 'lsp-ui-mode :async t
    :definition 'lsp-ui-peek-find-definitions
    :references 'lsp-ui-peek-find-references))


(def-package! company-lsp
  :when (featurep! :completion company)
  :defer t
  :init
  ;; Make sure that `company-capf' is disabled since it is incompatible with
  ;; `company-lsp' (see lsp-mode#884)
  (defun +lsp|init-company ()
    (if (not (bound-and-true-p company-mode))
        (add-hook 'company-mode-hook #'+lsp|init-company t t)
      (setq-local company-backends
                    (cons 'company-lsp
                          (remq 'company-capf company-backends)))
      (remove-hook 'company-mode-hook #'+lsp|init-company t)))
  (add-hook 'lsp-mode-hook #'+lsp|init-company))
