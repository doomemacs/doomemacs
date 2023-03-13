;;; tools/lsp/autoload/common.el -*- lexical-binding: t; -*-

;;;###autodef (fset 'lsp! #'ignore)
(defun lsp! ()
  "Dispatch to call the currently used lsp client entrypoint"
  (interactive)
  (cond
   ((modulep! +lsp-bridge) nil)
   ((modulep! +eglot) (eglot-ensure))
   ((unless (bound-and-true-p lsp-mode) (lsp-deferred)))))
