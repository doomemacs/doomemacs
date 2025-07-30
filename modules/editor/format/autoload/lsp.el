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
                  (cl-remove-duplicates (cons 'lsp (ensure-list +format-with))
                                        :test #'eq)
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
            ((apply fn (car +format--region-p) (cdr +format--region-p)
                    plist)))
      (funcall callback)
    (funcall callback "LSP server doesn't support formatting")))

(cl-defun +format--with-lsp-mode (beg end &key buffer scratch callback &allow-other-keys)
  "Format the current buffer or region with any available lsp-mode formatter.

Won't forward the buffer to chained formatters if successful."
  (with-current-buffer buffer
    (let ((edits
           (cond ((and (null beg) (lsp-feature? "textDocument/formatting"))
                  (lsp-request "textDocument/formatting"
                               (lsp--make-document-formatting-params)))
                 ((lsp-feature? "textDocument/rangeFormatting")
                  (lsp-request "textDocument/rangeFormatting"
                               (lsp--make-document-range-formatting-params
                                (or beg (point-min)) (or end (point-max)))))
                 ;; try next chained formatter(s)
                 ((cl-return (ignore (funcall callback)))))))
      (unless (seq-empty-p edits)
        (with-current-buffer scratch
          (lsp--apply-text-edits edits 'format)))
      t)))

(cl-defun +format--with-eglot (beg end &key scratch buffer callback &allow-other-keys)
  "Format the current buffer or region with any available eglot formatter.

Won't forward the buffer to chained formatters if successful."
  (let ((edits
         (with-current-buffer buffer
           (pcase-let
               ((`(,method ,args)
                 (cond ((and (not beg) (eglot-server-capable :documentFormattingProvider))
                        '(:textDocument/formatting nil))
                       ((eglot-server-capable :documentRangeFormattingProvider)
                        `(:textDocument/rangeFormatting
                          (:range ,(list :start (eglot--pos-to-lsp-position (or beg (point-min)))
                                         :end   (eglot--pos-to-lsp-position (or end (point-max)))))))
                       ;; try next chained formatter(s)
                       ((cl-return (ignore (funcall callback)))))))
             (eglot--request
              (eglot--current-server-or-lose)
              method
              (cl-list*
               :textDocument (eglot--TextDocumentIdentifier)
               :options (list :tabSize tab-width
                              :insertSpaces (if indent-tabs-mode :json-false t)
                              :insertFinalNewline (if require-final-newline t :json-false)
                              :trimFinalNewlines (if delete-trailing-lines t :json-false))
               args))))))
    (unless (seq-empty-p edits)
      (with-current-buffer scratch
        (with-demoted-errors "%s"
          (eglot--apply-text-edits edits))))
    t))

;;; lsp.el ends here
