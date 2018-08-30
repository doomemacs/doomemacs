;;; editor/format/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(autoload 'format-all-probe "format-all")

(defun +format--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function.

Stolen shamelessly from go-mode"
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        ((delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

;;;###autoload
(defun +format--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer.

Stolen shamelessly from go-mode"
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in +format--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (1- (- from line-offset)))
                (cl-incf line-offset len)
                (+format--delete-whole-line len)))
             ((error "Invalid rcs patch or internal error in +format--apply-rcs-patch")))))))
    (move-to-column column)))

(defun +format--with-copy-of-buffer (formatter executable mode-result)
  "Run formatter in a copy of the current buffer.

Since `format-all' functions (and various formatting functions, like `gofmt')
widen the buffer, in order to only format a region of text, we must make a copy
of the buffer to apply formatting to."
  (if (buffer-narrowed-p)
      (let ((output (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-buffer
          (insert output)
          (funcall formatter executable mode-result)))
    (funcall formatter executable mode-result)))


;;
;; Public library
;;

;;;###autoload
(defun +format-buffer (formatter mode-result)
  "Format the source code in the current buffer.

Returns any of the following values:

  'unknown   No formatter is defined for this major-mode
  'error     Couldn't format buffer due to formatter errors
  'noop      Buffer is already formatted

Otherwise, returns a list: (list OUTPUT ERRORS FIRST-DIFF), where OUTPUT is the
formatted text, ERRORS are any errors in string format, and FIRST-DIFF is the
position of the first change in the buffer.

See `+format/buffer' for the interactive version of this function, and
`+format|buffer' to use as a `before-save-hook' hook."
  (unless formatter
    (user-error "Don't know how to format '%s' code" major-mode))
  (let ((f-function (gethash formatter format-all-format-table))
        (executable (format-all-formatter-executable formatter)))
    (cl-destructuring-bind (output errput first-diff)
        (+format--with-copy-of-buffer f-function executable mode-result)
      (unwind-protect
          (cond ((null output) 'error)
                ((eq output t) 'noop)
                ((let ((tmpfile (make-temp-file "doom-format"))
                       (patchbuf (get-buffer-create " *doom format patch*"))
                       (coding-system-for-read 'utf-8)
                       (coding-system-for-write 'utf-8))
                   (unwind-protect
                       (progn
                         (with-current-buffer patchbuf (erase-buffer))
                         (with-temp-file tmpfile (erase-buffer) (insert output))
                         (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                             'noop
                           (+format--apply-rcs-patch patchbuf)
                           (list output errput first-diff))))
                   (kill-buffer patchbuf)
                   (delete-file tmpfile))))
        (unless (= 0 (length errput))
          (message "Formatter error output:\n%s" errput))))))


;;
;; Commands
;;

;;;###autoload
(defun +format/buffer ()
  "Format the source code in the current buffer."
  (interactive)
  (+format|buffer))

;;;###autoload
(defun +format/region (beg end)
  "Runs the active formatter on the lines within BEG and END.

WARNING: this may not work everywhere. It will throw errors if the region
contains a syntax error in isolation. It is mostly useful for formatting
snippets or single lines."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (+format/buffer)))

;;;###autoload
(defun +format/region-or-buffer (beg end)
  "Runs the active formatter on the selected region (or whole buffer, if nothing
is selected)."
  (interactive "r")
  (if (use-region-p)
      (+format/region beg end)
    (+format/buffer)))


;;
;; Hooks
;;

;;;###autoload
(defun +format|enable-on-save ()
  "Enables formatting on save."
  (add-hook 'before-save-hook #'+format|buffer nil t))

;;;###autoload
(defun +format|buffer ()
  "Format the source code in the current buffer with minimal feedback.

Meant for `before-save-hook'."
  (cl-destructuring-bind (formatter mode-result) (format-all-probe)
    (pcase (+format-buffer formatter mode-result)
      (`error (message "Failed to format buffer due to errors"))
      (`noop (message "Buffer was already formatted"))
      (_ (message "Formatted (%s)" formatter)))))

