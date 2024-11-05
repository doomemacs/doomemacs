;;; tools/lsp/autoload/common.el -*- lexical-binding: t; -*-

;;;###autodef (fset 'lsp! #'ignore)
(defun lsp! ()
  "Dispatch to call the currently used lsp client entrypoint"
  (if (modulep! +eglot)
      (when (require 'eglot nil t)
        (if (eglot--lookup-mode major-mode)
            (eglot-ensure)
          (eglot--message "No client defined for %s" major-mode)))
    (unless (bound-and-true-p lsp-mode)
      (lsp-deferred))))
