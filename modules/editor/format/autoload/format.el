;;; editor/format/autoload.el -*- lexical-binding: t; -*-

(defun +format--current-indentation ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (current-indentation)))

;;;###autoload (autoload 'apheleia--get-formatters "apheleia-formatters")

;;;###autoload
(defvar +format--region-p nil)

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
    (unwind-protect
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
          (let ((+format--region-p (cons start end)))
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
                 (with-silent-modifications
                   (replace-region-contents start end (lambda () formatted-buffer) 5))
                 (when callback (funcall callback))
                 (kill-buffer formatted-buffer))))))
      (when (doom-region-active-p)
        (setq deactivate-mark t)))))


;;
;;; Commands

;;;###autoload
(defalias '+format/buffer #'apheleia-format-buffer)

;;;###autoload
(defun +format/region (beg end &optional _arg interactive)
  "Format the selected region.

WARNING: if the formatter doesn't support partial formatting, this command tries
to pretend the active selection is the contents of a standalone file, but this
may not always work. Keep your undo keybind handy!"
  (interactive (list (doom-region-beginning)
                     (doom-region-end)
                     current-prefix-arg
                     'interactive))
  (+format-region
   beg end
   (lambda ()
     (when interactive
       (message "Region reformatted!")))))

;;;###autoload
(defun +format/region-or-buffer ()
  "Format the selected region, or whole buffer if nothing is selected."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'+format/region
     #'+format/buffer)))

;;; format.el ends here
