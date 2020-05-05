;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

;;;###autoload
(defun +java/update-lsp-server ()
  (interactive)
  (lsp--install-server-internal (gethash 'jdtls lsp-clients) t))

(add-hook! java-mode-local-vars #'lsp!)
(map! :map java-mode-map
      :localleader
      :desc "Update the Eclipse LSP server" "U" #'+java/update-lsp-server)

(use-package! lsp-java
  :after (lsp-clients dap-mode)
  :preface
  (setq lsp-java-server-install-dir (concat doom-etc-dir "eclipse.jdt.ls/server/")
        lsp-java-workspace-dir (concat doom-etc-dir "java-workspace")))
