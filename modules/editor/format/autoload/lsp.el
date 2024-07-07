;;; editor/format/autoload/lsp.el -*- lexical-binding: t; -*-
;;;###if (modulep! :tools lsp)

(defvar +format-with--last nil)

;;;###autoload
(defun +format-enable-lsp-formatter-h ()
  "TODO"
  (when (and +format-with-lsp (null +format-with))
    (when (or (lsp-feature? "textDocument/formatting")
              (lsp-feature? "textDocument/rangeFormatting"))
      (setq-local +format-with--last +format-with
                  +format-with 'lsp))))

;;;###autoload
(defun +format-disable-lsp-formatter-h ()
  "TODO"
  (when (local-variable-p '+format-with--last)
    (kill-local-variable '+format-with--last)
    (setq-local +format-with +format-with--last)))

;;;###autoload
(defun +format-toggle-eglot-formatter-h ()
  "TODO"
  (if (bound-and-true-p eglot--managed-mode)
      (when (and +format-with-lsp (null +format-with))
        (when (or (eglot--server-capable :documentFormattingProvider)
                  (eglot--server-capable :documentRangeFormattingProvider))
          (setq-local +format-with--last +format-with
                      +format-with 'eglot)))
    (when +format-with--last
      (kill-local-variable '+format-with--last)
      (setq-local +format-with +format-with--last))))


;;
;;; Apheleia formatters

;;;###autoload
(cl-defun +format-lsp-buffer (&key buffer scratch callback &allow-other-keys)
  "Format the current buffer with any available lsp-mode formatter."
  (with-current-buffer buffer
    (let ((edits
           (cond
            ((lsp-feature? "textDocument/formatting")
             (lsp-request "textDocument/formatting" (lsp--make-document-formatting-params)))
            ((lsp-feature? "textDocument/rangeFormatting")
             (lsp-request "textDocument/rangeFormatting"
                          (lsp--make-document-range-formatting-params
                           (point-min) (point-max)))))))
      (unless (seq-empty-p edits)
        (with-current-buffer scratch
          (lsp--apply-text-edits edits 'format))))
    (funcall callback)))

;;;###autoload
(cl-defun +format-eglot-buffer (&key buffer scratch callback &allow-other-keys)
  "Format the current buffer with any available eglot formatter."
  (with-current-buffer scratch
    (setq-local eglot--cached-server
                (with-current-buffer buffer
                  (eglot-current-server)))
    (let ((buffer-file-name (buffer-file-name buffer)))
      (eglot-format-buffer))
    (funcall callback)))

;;; lsp.el ends here
