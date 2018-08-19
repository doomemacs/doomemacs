;;; feature/eval/autoload/repl.el -*- lexical-binding: t; -*-

(defvar +eval-repl-buffers-alist ()
  "The buffer of the last open repl.")

(define-minor-mode +eval-repl-mode
  "A minor mode for REPL buffers.")

(defun +eval--ensure-in-repl-buffer (&optional command same-window-p)
  (setq +eval-repl-buffers-alist
        (cl-remove-if-not #'buffer-live-p +eval-repl-buffers-alist
                          :key #'cdr))
  (let* ((project-root (doom-project-root 'nocache))
         (key (cons major-mode project-root))
         (buffer (cdr (assoc key +eval-repl-buffers-alist))))
    (unless (and (bufferp buffer)
                 (eq buffer (current-buffer)))
      (funcall (if same-window-p #'switch-to-buffer #'pop-to-buffer)
               (if (buffer-live-p buffer)
                   buffer
                 (setq buffer
                       (save-window-excursion
                         (if (commandp command)
                             (call-interactively command)
                           (funcall command))))
                 (unless (bufferp buffer)
                   (error "REPL command didn't return a buffer"))
                 (with-current-buffer buffer (+eval-repl-mode +1))
                 (setf (alist-get key +eval-repl-buffers-alist nil nil #'equal)
                       buffer)
                 buffer)))
    (with-current-buffer buffer
      (goto-char (if (and (derived-mode-p 'comint-mode)
                          (cdr comint-last-prompt))
                     (cdr comint-last-prompt)
                   (point-max)))
      buffer)))

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
