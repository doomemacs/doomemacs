;;; feature/eval/autoload/repl.el -*- lexical-binding: t; -*-

(defvar +eval-repl-buffer nil
  "The buffer of the last open repl.")

(defun +eval--ensure-in-repl-buffer (&optional command same-window-p)
  (cond ((eq (current-buffer) +eval-repl-buffer))
        ((and +eval-repl-buffer
              (buffer-live-p +eval-repl-buffer))
         (when-let* ((win (get-buffer-window +eval-repl-buffer)))
           (select-window win)))
        (command
         (let ((repl-buffer (save-window-excursion (call-interactively command))))
           (unless (bufferp repl-buffer)
             (error "REPL command didn't return a buffer"))
           (with-current-buffer repl-buffer (+eval-repl-mode +1))
           (setq +eval-repl-buffer repl-buffer))))
  (unless (eq (current-buffer) +eval-repl-buffer)
    (funcall (if same-window-p #'switch-to-buffer #'pop-to-buffer)
             +eval-repl-buffer))
  (with-current-buffer +eval-repl-buffer
    (goto-char (if (and (derived-mode-p 'comint-mode)
                        (cdr comint-last-prompt))
                   (cdr comint-last-prompt)
                 (point-max)))
    t))

;;;###autoload
(defun +eval/open-repl (&optional same-window-p)
  "Opens (or reopens) the REPL associated with the current major-mode and place
the cursor at the prompt.

If SAME-WINDOW-P is non-nil, open REPL in current window."
  (interactive "P")
  (if-let* ((command (cdr (assq major-mode +eval-repls))))
      (when (+eval--ensure-in-repl-buffer command same-window-p)
        (when (bound-and-true-p evil-mode)
          (call-interactively #'evil-append-line))
        t)
    (user-error "No REPL is defined for %s" major-mode)))

;;;###autoload
(defun +eval/send-region-to-repl (beg end &optional auto-execute-p)
  "REPL must be open! Sends a selected region to it. If AUTO-EXECUTE-P, then
execute it immediately after."
  (interactive "r")
  (let ((selection (buffer-substring-no-properties beg end)))
    (unless (+eval--ensure-in-repl-buffer)
      (error "No REPL open"))
    (when (bound-and-true-p evil-mode)
      (call-interactively #'evil-append-line))
    (insert (string-trim selection))
    (when auto-execute-p
      ;; I don't use `comint-send-input' because different REPLs may have their
      ;; own. So I just emulate the keypress.
      (execute-kbd-macro (kbd "RET")))))
