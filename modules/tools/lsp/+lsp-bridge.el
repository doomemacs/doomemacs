;;; tools/lsp/+lsp-bridge.el -*- lexical-binding: t; -*-

(use-package! lsp-bridge
  :commands lsp-bridge-install-tabnine lsp-bridge-install-omnisharp
  :config
  (set-lookup-handlers! 'lsp-bridge-mode
    :definition #'lsp-bridge-find-def
    :references #'lsp-find-references
    :implementations #'lsp-bridge-find-impl
    :type-definition #'lsp-bridge-find-def
    :documentation #'lsp-bridge-popup-documentation
    :async t)

  ;; Disable company backends when using lsp-bridge since it
  ;; provides its own completion framework.
  (when (modulep! :completion company)
    (set-company-backend! 'lsp-bridge nil))

  (defadvice! +lsp--defer-server-shutdown-a (fn)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'lsp-bridge-kill-process
    (letf! (defun lsp-bridge-shutdown ()
             (if (or (null +lsp-defer-shutdown)
                     (eq +lsp-defer-shutdown 0))
                 (prog1 (funcall lsp-bridge-shutdown)
                   (+lsp-optimization-mode -1))
               (run-at-time
                (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
                nil (lambda ()
                      (prog1 (funcall lsp-bridge-shutdown)
                        (+lsp-optimization-mode -1))))))
      (funcall fn)))
  (global-lsp-bridge-mode))
