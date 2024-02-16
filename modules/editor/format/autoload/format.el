;;; editor/format/autoload.el -*- lexical-binding: t; -*-

(defun +format--current-indentation ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (current-indentation)))

(defun +format-region (start end &optional callback)
  "Format from START to END with `apheleia'."
  (when-let* ((command (apheleia--get-formatters
                        (if current-prefix-arg
                            'prompt
                          'interactive)))
              (cur-buffer (current-buffer))
              (formatted-buffer (get-buffer-create " *apheleia-formatted*"))
              (indent 0))
    (with-current-buffer formatted-buffer
      (erase-buffer)
      (unless (featurep :system 'windows)
        (setq-local coding-system-for-read 'utf-8)
        (setq-local coding-system-for-write 'utf-8))
      ;; Ensure this temp buffer seems as much like the origin buffer as
      ;; possible, in case the formatter is an elisp function, like `gofmt'.
      (cl-loop for (var . val)
               in (cl-remove-if-not #'listp (buffer-local-variables cur-buffer))
               ;; Making enable-multibyte-characters buffer-local causes an
               ;; error.
               unless (eq var 'enable-multibyte-characters)
               ;; Using setq-local would quote var.
               do (set (make-local-variable var) val))
      ;;
      (insert-buffer-substring-no-properties cur-buffer start end)
      ;; Since we're piping a region of text to the formatter, remove any
      ;; leading indentation to make it look like a file.
      (setq indent (+format--current-indentation))
      (when (> indent 0)
        (indent-rigidly (point-min) (point-max) (- indent)))
      ;;
      (apheleia-format-buffer
       command
       (lambda ()
         (with-current-buffer formatted-buffer
           (when (> indent 0)
             ;; restore indentation without affecting new
             ;; indentation
             (indent-rigidly (point-min) (point-max)
                             (max 0 (- indent (+format--current-indentation)))))
           (set-buffer-modified-p nil))
         (with-current-buffer cur-buffer
           (delete-region start end)
           (insert-buffer-substring-no-properties formatted-buffer)
           (when callback (funcall callback))
           (kill-buffer formatted-buffer)))))))


;;
;;; Commands

;;;###autoload
(defun +format/buffer (&optional arg)
  "Reformat the current buffer using LSP or `format-all-buffer'."
  (interactive "P")
  (or (run-hook-with-args-until-success '+format-functions (point-min) (point-max) 'buffer)
      (call-interactively #'apheleia-format-buffer)))

;;;###autoload
(defun +format/region (beg end &optional arg)
  "Runs the active formatter on the lines within BEG and END.

WARNING: this may not work everywhere. It will throw errors if the region
contains a syntax error in isolation. It is mostly useful for formatting
snippets or single lines."
  (interactive "rP")
  (or (run-hook-with-args-until-success '+format-functions beg end 'region)
      (+format-region beg end)))

;;;###autoload
(defun +format/region-or-buffer ()
  "Runs the active formatter on the selected region (or whole buffer, if nothing
is selected)."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'+format/region
     #'+format/buffer)))


;;
;;; Specialized formatters

;;;###autoload
(defun +format-with-lsp-fn (beg end op)
  "Format the region/buffer using any available lsp-mode formatter.

Does nothing if `+format-with-lsp' is nil or the active server doesn't support
the requested feature."
  (and +format-with-lsp
       (bound-and-true-p lsp-mode)
       (pcase op
         ('buffer (condition-case _
                      ;; Avoid lsp-feature? checks for this, since
                      ;; `lsp-format-buffer' does its own, and allows clients
                      ;; without formatting support (but with rangeFormatting,
                      ;; for some reason) to work.
                      (always (lsp-format-buffer))
                    ('lsp-capability-not-supported nil)))
         ('region (if (lsp-feature? "textDocument/rangeFormatting")
                      (always (lsp-format-region beg end))))
         (_ (error "Invalid formatter operation: %s" op)))))

;;;###autoload
(defun +format-with-eglot-fn (beg end op)
  "Format the region/buffer using any available eglot formatter.

Does nothing if `+format-with-lsp' is nil or the active server doesn't support
the requested feature."
  (and +format-with-lsp
       (bound-and-true-p eglot-managed-mode)
       (pcase op
         ('buffer (if (eglot--server-capable :documentFormattingProvider)
                      (always (eglot-format-buffer))))
         ('region (if (eglot--server-capable :documentRangeFormattingProvider)
                      (always (eglot-format beg end))))
         (_ (error "Invalid formatter operation: %s" op)))))

;;;###autoload
(defun +format-in-org-src-blocks-fn (beg end _op)
  "Reformat org src blocks with apheleia as if they were independent buffers."
  (when (derived-mode-p 'org-mode)
    (goto-char beg)
    (while (re-search-forward org-babel-src-block-regexp end t)
      (let* ((element (org-element-at-point))
             (block-beg (save-excursion
                          (goto-char (org-babel-where-is-src-block-head element))
                          (line-beginning-position 2)))
             (block-end (save-excursion
                          (goto-char (org-element-property :end element))
                          (skip-chars-backward " \t\n")
                          (line-beginning-position)))
             (beg (max beg block-beg))
             (end (min end block-end))
             (lang (org-element-property :language element))
             (major-mode (org-src-get-lang-mode lang)))
        (save-excursion
          (if (eq major-mode 'org-mode)
              (user-error "Cannot reformat an org src block in org-mode")
            ;; Determine formatter based on language and format the region
            (let ((formatter (apheleia--get-formatters 'interactive)))
              (unless formatter
                (setq formatter (apheleia--get-formatters 'prompt))
                (unless formatter
                  (user-error "No formatter configured for language: %s" lang)))
              (let ((apheleia-formatter formatter))
                (+format-region beg end)))))))
    t))
