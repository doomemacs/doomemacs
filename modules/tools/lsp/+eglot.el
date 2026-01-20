;;; tools/lsp/+eglot.el -*- lexical-binding: t; -*-

(use-package! eglot
  :commands eglot eglot-ensure
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :init
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        ;; NOTE: We disable eglot-auto-display-help-buffer because :select t in
        ;;   its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil
        ;; Leave it to our modules and user config to initialize these.
        eglot-stay-out-of
        (append (if (modulep! :checkers syntax -flymake)
                    '(flymake))
                (if (modulep! :completion company)
                    '(company)))
        ;; Margin indicator may increase line height due to glyph display
        ;; failures or emoji font height differences; I also think the eldoc
        ;; hint is enough.
        eglot-code-action-indications '(eldoc-hint))

  :config
  (set-popup-rule! "^\\*eglot-help" :size 0.15 :quit t :select t)
  (set-lookup-handlers! 'eglot--managed-mode
    :definition      #'xref-find-definitions
    :references      #'xref-find-references
    :implementations #'eglot-find-implementation
    :type-definition #'eglot-find-typeDefinition
    :documentation   #'+eglot-lookup-documentation)

  ;; Leave management of flymake to the :checkers syntax module.
  (when (modulep! :checkers syntax -flymake)
    (add-to-list 'eglot-stay-out-of 'flymake))

  ;; PERF: Disable the eglot-events-buffer, so Emacs doesn't churn GC and CPU
  ;;   cycles on pretty-printing the events buffer in the background (once it
  ;;   reaches max size). Enable debug mode to restore the events buffer.
  (cl-callf plist-put eglot-events-buffer-config :size 0)

  (set-debug-variable! 'eglot-events-buffer-config '(:size 2000000 :format full))

  (defadvice! +lsp--defer-server-shutdown-a (fn &optional server)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also spares the
server an expensive restart when its buffer is reverted."
    :around #'eglot--managed-mode
    (letf! (defun eglot-shutdown (server)
             (if (or (null +lsp-defer-shutdown)
                     (eq +lsp-defer-shutdown 0))
                 (prog1 (funcall eglot-shutdown server)
                   (+lsp-optimization-mode -1))
               (run-at-time
                (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
                nil (lambda (server)
                      (unless (eglot--managed-buffers server)
                        (prog1 (funcall eglot-shutdown server)
                          (+lsp-optimization-mode -1))))
                server)))
      (funcall fn server))))


(use-package! eglot-booster
  :when (modulep! +booster)
  :after eglot
  :init
  (setq eglot-booster-io-only
        ;; JSON parser on 30+ is faster, so we only exploit eglot-booster's IO
        ;; buffering (benefits more talkative LSP servers).
        (and (> emacs-major-version 29)
             (not (functionp 'json-rpc-connection))))
  :config
  (eglot-booster-mode +1))


(use-package! consult-eglot
  :when (modulep! :completion vertico)
  :defer t
  :init
  (map! :after eglot
        :map eglot-mode-map
        [remap xref-find-apropos] #'consult-eglot-symbols))


(use-package! flycheck-eglot
  :when (modulep! :checkers syntax -flymake)
  :hook (eglot-managed-mode . flycheck-eglot-mode))
