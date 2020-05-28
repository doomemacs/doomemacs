;;; tools/lsp/+eglot.el -*- lexical-binding: t; -*-

(use-package! eglot
  :commands (eglot-ensure eglot)
  :when (featurep! +eglot)
  :init
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        ;; NOTE: Do NOT set eglot-auto-display-help-buffer to t.
        ;; With popup-rule! :select t, eglot will steal focus from the source code very often.
        eglot-auto-display-help-buffer nil)
  :config
  (set-popup-rule! "^\\*eglot-help" :size 0.35 :quit t :select t)
  (when (featurep! :checkers syntax)
    (after! flycheck
      (load! "autoload/flycheck-eglot.el")))
  (set-lookup-handlers! 'eglot--managed-mode
    :documentation #'+eglot/documentation-lookup-handler))
