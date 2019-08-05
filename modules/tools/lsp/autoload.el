;;; feature/lsp/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defun lsp! (&optional arg)
  "Enable `lsp-mode' in the current buffer.

Meant to be a lighter alternative to `lsp', which is too eager about
initializing lsp-ui-mode, company, yasnippet and flycheck. Instead, these have
been moved out to their respective modules, or these hooks:

+ `+lsp|init-company' (on `lsp-mode-hook')
+ `+lsp|init-ui-flycheck-or-flymake' (on `lsp-ui-mode-hook')"
  (require 'lsp-mode)
  (unless lsp-mode
    (when lsp-auto-configure
      (require 'lsp-clients))
    (when (and (buffer-file-name)
               (setq-local lsp--buffer-workspaces
                           (or (lsp--try-open-in-library-workspace)
                               (lsp--try-project-root-workspaces (equal arg '(4))
                                                                 (and arg (not (equal arg 1)))))))
      (lsp-mode 1)
      (lsp--info
       "Connected to %s."
       (apply #'concat (mapcar (lambda (it) (format "[%s]" (lsp--workspace-print it)))
                               lsp--buffer-workspaces))))))
