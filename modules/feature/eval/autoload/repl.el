;;; feature/eval/autoload/repl.el -*- lexical-binding: t; -*-

(defvar +eval-repl-buffer nil
  "The buffer of the last open repl.")

(defun +eval--ensure-in-repl-buffer (&optional command)
  (or (eq (current-buffer) +eval-repl-buffer)
      (progn
        (if (and +eval-repl-buffer (buffer-live-p +eval-repl-buffer))
            (if-let (win (get-buffer-window +eval-repl-buffer))
                (select-window win)
              (doom-popup-buffer +eval-repl-buffer))
          (when command
            (let ((repl-buffer (save-window-excursion (call-interactively command))))
              (unless (bufferp repl-buffer)
                (error "REPL command didn't return a buffer"))
              (with-current-buffer repl-buffer (+eval-repl-mode +1))
              (setq +eval-repl-buffer repl-buffer)
              (select-window (doom-popup-buffer repl-buffer)))))
        (when (eq (current-buffer) +eval-repl-buffer)
          (goto-char (if (and (derived-mode-p 'comint-mode)
                              (cdr comint-last-prompt))
                         (cdr comint-last-prompt)
                       (point-max)))
          t))))

;;;###autoload
(defun +eval/repl ()
  "Opens (or reopens) the REPL associated with the current major-mode and place
the cursor at the prompt."
  (interactive)
  (when-let (command (cdr (assq major-mode +eval-repls)))
    (when (+eval--ensure-in-repl-buffer command)
      (when (bound-and-true-p evil-mode)
        (call-interactively #'evil-append-line))
      t)))

;;;###autoload
(defun +eval/repl-send-region (beg end &optional auto-execute-p)
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
