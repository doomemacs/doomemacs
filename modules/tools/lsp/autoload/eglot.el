;;; tools/lsp/autoload/eglot.el -*- lexical-binding: t; -*-
;;;###if (featurep! +eglot)

;;;###autodef
(defun set-eglot-client! (mode server-call)
  "Add SERVER-CALL list as a possible lsp server for given major MODE.

Example : (set-eglot-client! 'python-mode `(,(concat doom-etc-dir \"lsp/mspyls/Microsoft.Python.LanguageServer\")))"
  (after! eglot
    (add-to-list 'eglot-server-programs `(,mode . ,server-call))))

;;;###autoload
(defun +eglot/documentation-lookup-handler ()
  "Documentation lookup handler using eglot :document/hover handler.

Mostly a rewrite of `eglot-help-at-point', which should be used interactively."
  (interactive)
  (eglot-help-at-point)
  (display-buffer eglot--help-buffer))
