;;; tools/lsp/+eglot.el -*- lexical-binding: t; -*-

;; TODO set eglot-events-buffer-size to nil in doom-debug-mode
;; TODO Implement `+lsp-defer-shutdown'

(use-package! eglot
  :commands eglot eglot-ensure
  :hook (eglot-managed-mode . +lsp-init-optimizations-h)
  :init
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil)

  :config
  (set-popup-rule! "^\\*eglot-help" :size 0.35 :quit t :select t)
  (set-lookup-handlers! 'eglot--managed-mode
    :implementations #'eglot-find-implementation
    :type-definition #'eglot-find-typeDefinition
    :documentation #'+eglot/documentation-lookup-handler)
  (when (featurep! :checkers syntax)
    (after! flycheck
      (load! "autoload/flycheck-eglot"))))
