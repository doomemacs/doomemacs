;;; editor/format/autoload/lsp.el -*- lexical-binding: t; -*-
;;;###if (modulep! :tools lsp)

(defvar +format-lsp--last nil)
;;;###autoload
(define-minor-mode +format-with-lsp-mode
  "Toggles `lsp-mode'/`eglot' integration with `apheleia-mode' in this buffer.

When enabled, it changes `+format-with' to `lsp', and back to its old value when
disabled. Use `+format-wtih-lsp-maybe-h' to activate it, to ensure it only
activates if lsp-mode/eglot are on and connected to a valid client, and so this
mode won't conflict with pre-existing user config on `+format-with'."
  :init-value nil
  (unless (local-variable-p '+format-lsp--last)
    (setq-local +format-lsp--last +format-with))
  (setq-local +format-with
              (if +format-with-lsp-mode
                  (cl-remove-duplicates (cons 'lsp +format-with) :test #'eq)
                (prog1 (remq 'lsp (ensure-list +format-lsp--last))
                  (kill-local-variable '+format-lsp--last)))))

(defvar +format--lsp-alist
  '((lsp-managed-mode . +format--with-lsp-mode)
    (eglot--managed-mode . +format--with-eglot))
  "A list of LSP formatter functions to try on `+format-lsp-buffer'.")

(defun +format--lsp-fn ()
  (cl-loop for (sym . fn) in +format--lsp-alist
           if (and (boundp sym) (symbol-value sym))
           return fn))

;;;###autoload
(defun +format-with-lsp-toggle-h ()
  "Toggle `+format-with-lsp-mode' depending on the state of lsp-mode/eglot.

Does not activate the mode if `+format-with' is already set. To activate the
mode unconditionally, call `+format-with-lsp-mode' instead."
  (when (or (null +format-with) +format-with-lsp-mode)
    (+format-with-lsp-mode (if (+format--lsp-fn) +1 -1))))


;;
;;; Apheleia formatters

;;;###autoload
(cl-defun +format-lsp-buffer (&rest plist &key buffer callback &allow-other-keys)
  "Format the current buffer with any available lsp-mode or eglot formatter."
  (if-let* ((fn (with-current-buffer buffer (+format--lsp-fn)))
            ((apply fn plist)))
      (funcall callback)
    (funcall callback "LSP server doesn't support formatting")))

(cl-defun +format--with-lsp-mode (&key buffer scratch &allow-other-keys)
  "Format the current buffer with any available lsp-mode formatter."
  (with-current-buffer buffer
    (let ((edits
           (cond
            ((lsp-feature? "textDocument/formatting")
             (lsp-request "textDocument/formatting"
                          (lsp--make-document-formatting-params)))
            ((lsp-feature? "textDocument/rangeFormatting")
             (lsp-request "textDocument/rangeFormatting"
                          (lsp--make-document-range-formatting-params
                           (point-min) (point-max))))
            (:err))))
      (unless (eq edits :err)
        (unless (seq-empty-p edits)
          (with-current-buffer scratch
            (lsp--apply-text-edits edits 'format)))
        t))))

(cl-defun +format--with-eglot (&key buffer scratch &allow-other-keys)
  "Format the current buffer with any available eglot formatter."
  (with-current-buffer scratch
    (when (setq-local
           eglot--cached-server
           (with-current-buffer buffer
             (when (or (eglot-server-capable :documentFormattingProvider)
                       (eglot-server-capable :documentRangeFormattingProvider))
               (eglot-current-server))))
      (let ((buffer-file-name (buffer-file-name buffer)))
        (eglot-format-buffer))
      t)))

;;; lsp.el ends here
