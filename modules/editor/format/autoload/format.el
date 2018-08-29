;;; editor/format/autoload.el -*- lexical-binding: t; -*-

;; Stolen shamelessly from go-mode
(defun +format--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
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

;; Stolen shamelessly from go-mode
;;;###autoload
(defun +format--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
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


;;
;; Public library
;;

;;;###autoload
(defun +format-buffer ()
  "Auto-format the source code in the current buffer."
  (interactive)
  (require 'format-all)
  (cl-destructuring-bind (formatter mode-result) (format-all-probe)
    (unless formatter
      (error "Don't know how to format %S code" major-mode))
    (let ((f-function (gethash formatter format-all-format-table))
          (executable (format-all-formatter-executable formatter)))
      (cl-destructuring-bind (output errput first-diff)
          (funcall f-function executable mode-result)
        (prog1 (cl-case output
                 ((nil) 'error)
                 ((t) 'noop)
                 (t (erase-buffer)
                    (insert output)
                    (list output errput first-diff)))
          (with-current-buffer (get-buffer-create "*format-all-errors*")
            (erase-buffer)
            (unless (= 0 (length errput))
              (insert errput)
              (display-buffer (current-buffer)))))))))


;;
;; Commands
;;

;;;###autoload
(defun +format/buffer ()
  "TODO"
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
  "TODO"
  (let ((tmpfile (make-temp-file "doom_format"))
        (patchbuf (get-buffer-create " *doom format patch*"))
        (mode major-mode)
        (file buffer-file-name)
        (dir default-directory)
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (unwind-protect
        (save-restriction
          (with-current-buffer patchbuf (erase-buffer))
          (quiet! (write-region (point-min) (point-max) tmpfile))
          (pcase (with-current-buffer (find-file-noselect tmpfile t)
                   (delay-mode-hooks (funcall mode))
                   (setq buffer-file-name file
                         default-directory dir)
                   (quiet! (+format-buffer))
                   (quiet! (write-region nil nil tmpfile)))
            (`noop  (message "Buffer is already formatted"))
            (`error (message "Couldn't format the buffer due to errors"))
            (_ (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                   (message "Buffer is already formatted")
                 (+format--apply-rcs-patch patchbuf)
                 (message "Formatted buffer")))))
      (kill-buffer patchbuf)
      (delete-file tmpfile))))

