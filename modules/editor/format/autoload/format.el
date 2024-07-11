;;; editor/format/autoload.el -*- lexical-binding: t; -*-

(defun +format--current-indentation ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (current-indentation)))

;;;###autoload (autoload 'apheleia--get-formatters "apheleia-formatters")

;;;###autoload
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
               ;; `enable-multibyte-characters' can change how Emacs reads the
               ;; buffer's contents (or writes them to the formatters), which
               ;; can cause errors.
               unless (eq var 'enable-multibyte-characters)
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
             ;; restore indentation without affecting new indentation
             (indent-rigidly (point-min) (point-max)
                             (max 0 (- indent (+format--current-indentation)))))
           (set-buffer-modified-p nil))
         (with-current-buffer cur-buffer
           (delete-region start end)
           (goto-char start)
           (save-excursion
             (insert-buffer-substring-no-properties formatted-buffer)
             (when callback (funcall callback)))
           (kill-buffer formatted-buffer)))))))

;;
;;; Commands

;;;###autoload
(defun +format/buffer (&optional arg)
  "Reformat the current buffer using `apheleia-format-buffer`.

If the current major mode is `org-mode`, reformat the current org src block instead."
  (interactive "P")
  (if (eq major-mode 'org-mode)
      (+format/org-block)
    (call-interactively #'apheleia-format-buffer)))

;;;###autoload
(defun +format/region (beg end &optional _arg)
  "Format the selected region.

WARNING: if the formatter doesn't support partial formatting, this command tries
to pretend the active selection is the contents of a standalone file, but this
may not always work. Keep your undo keybind handy!"
  (interactive "rP")
  (+format-region beg end))

;;;###autoload
(defun +format/region-or-buffer ()
  "Format the selected region, or whole buffer if nothing is selected."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'+format/region
     #'+format/buffer)))

;;;###autoload
(defun +format/org-block (&optional point)
  "Reformat the org src block at POINT with a mode approriate formatter."
  (interactive (list (point)))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org-mode buffer!"))
  (let ((element (org-element-at-point point)))
    (unless (org-in-src-block-p nil element)
      (user-error "Not in an org src block"))
    (cl-destructuring-bind (beg end _) (org-src--contents-area element)
      (let* ((lang (org-element-property :language element))
             (mode (org-src-get-lang-mode lang)))
        (save-excursion
          (if (provided-mode-derived-p mode 'org-mode)
              (user-error "Cannot reformat an org-mode or org-derived src block")
            (let* ((major-mode mode)
                   (after-change-functions
                    ;; HACK: Silence excessive and unhelpful warnings about
                    ;;   'org-element-at-point being used in non-org-mode
                    ;;   buffers'.
                    (remq 'org-indent-refresh-maybe after-change-functions))
                   (apheleia-formatter
                    (or (apheleia--get-formatters 'interactive)
                        (apheleia--get-formatters 'prompt)
                        (user-error "No formatter configured for language: %s" lang))))
              (+format-region beg end))))))))
