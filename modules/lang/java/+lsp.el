;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

;;;###autoload
(defun +java/update-lsp-server ()
  (interactive)
  (lsp--install-server-internal (gethash 'jdtls lsp-clients) t))

(add-hook! java-mode-local-vars #'lsp!)
(map! :map java-mode-map
      :localleader
      :desc "Update the Eclipse LSP server" "U" #'+java/update-lsp-server
      :desc "Open the test browser" "T" #'lsp-jt-browser)

;; TODO: This fails on the first java buffer opened - complains about the LSP server not having executeCode capabilities.
(defun java--lsp-jt-lens-mode-hook ()
  (when (derived-mode-p 'java-mode) (lsp-jt-lens-mode)))

(use-package! lsp-java
  :after (lsp-clients dap-mode)
  :hook (lsp-mode . java--lsp-jt-lens-mode-hook)
  :preface
  (setq lsp-java-server-install-dir (concat doom-etc-dir "eclipse.jdt.ls/server/")
        lsp-java-workspace-dir (concat doom-etc-dir "java-workspace")
        lsp-jt-root (concat doom-etc-dir "eclipse.jdt.ls/server/java-test/server")))
