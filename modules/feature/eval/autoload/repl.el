;;; feature/eval/autoload/repl.el

(defvar +eval-last-repl-buffer nil
  "The buffer of the last open repl.")

;;;###autoload
(defun +eval/repl ()
  "Open the REPL associated with the current major-mode. If selection is active,
send region to repl."
  (interactive)
  (when-let (command (cdr (assq major-mode +eval-repls)))
    (let ((repl-buffer (save-window-excursion (funcall command))))
      (unless (bufferp repl-buffer)
        (error "REPL command didn't return a buffer"))
      (with-current-buffer repl-buffer (+eval-repl-mode +1))
      (setq +eval-last-repl-buffer repl-buffer)
      (doom-popup-buffer repl-buffer))))

;;;###autoload
(defun +eval/repl-send-region (beg end &optional inhibit-run-p)
  "Send a selection to the REPL."
  (interactive "r")
  (when +eval-last-repl-buffer
    (let ((selection (buffer-substring-no-properties beg end)))
      (select-window (get-buffer-window +eval-last-repl-buffer))
      (goto-char (point-max))
      (insert selection)
      (when (and (featurep 'evil) evil-mode)
        (evil-change-state 'insert)))))

