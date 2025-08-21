;;; tools/lsp/+eglot.el -*- lexical-binding: t; -*-

(use-package! eglot
  :commands eglot eglot-ensure
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :init
  (setq eglot-sync-connect 1
        eglot-autoshutdown t
        ;; NOTE: We disable eglot-auto-display-help-buffer because :select t in
        ;;   its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil)
  (when (modulep! :checkers syntax -flymake)
    (setq eglot-stay-out-of '(flymake)))

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

  ;; NOTE: This setting disable the eglot-events-buffer enabling more consistent
  ;;   performance on long running emacs instance. Default is 2000000 lines.
  ;;   After each new event the whole buffer is pretty printed which causes
  ;;   steady performance decrease over time. CPU is spent on pretty priting and
  ;;   Emacs GC is put under high pressure.
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
