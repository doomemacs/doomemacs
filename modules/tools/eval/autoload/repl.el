;;; tools/eval/autoload/repl.el -*- lexical-binding: t; -*-

(defvar +eval-repl-buffers (make-hash-table :test 'equal)
  "The buffer of the last open repl.")

(define-minor-mode +eval-repl-mode
  "A minor mode for REPL buffers.")

(defun +eval--ensure-in-repl-buffer (&optional command other-window-p)
  (maphash (lambda (key buffer)
             (unless (buffer-live-p buffer)
               (remhash key +eval-repl-buffers)))
           +eval-repl-buffers)
  (let* ((project-root (doom-project-root))
         (key (cons major-mode project-root))
         (buffer (gethash key +eval-repl-buffers)))
    (cl-check-type buffer (or buffer null))
    (unless (eq buffer (current-buffer))
      (funcall (if other-window-p #'pop-to-buffer #'switch-to-buffer)
               (if (buffer-live-p buffer)
                   buffer
                 (setq buffer
                       (save-window-excursion
                         (if (commandp command)
                             (call-interactively command)
                           (funcall command))))
                 (cond ((null buffer)
                        (error "REPL handler %S couldn't open the REPL buffer" command))
                       ((not (bufferp buffer))
                        (error "REPL handler %S failed to return a buffer" command)))
                 (with-current-buffer buffer
                   (+eval-repl-mode +1))
                 (puthash key buffer +eval-repl-buffers)
                 buffer)))
    (with-current-buffer buffer
      (goto-char (if (and (derived-mode-p 'comint-mode)
                          (cdr comint-last-prompt))
                     (cdr comint-last-prompt)
                   (point-max)))
      buffer)))

(defun +eval-open-repl (prompt-p &optional other-window-p)
  (let ((command (cdr (assq major-mode +eval-repls))))
    (when (or (not command) prompt-p)
      (let* ((choices (or (cl-loop for sym being the symbols
                                   for sym-name = (symbol-name sym)
                                   if (string-match "^\\(?:\\+\\)?\\([^/]+\\)/open-\\(?:\\(.+\\)-\\)?repl$" sym-name)
                                   collect
                                   (format "%s (%s)"
                                           (match-string-no-properties 1 sym-name)
                                           (or (match-string-no-properties 2 sym-name) "default")))
                          (user-error "There are no known available REPLs")))
             (choice (or (completing-read "Open a REPL for: " choices)
                         (user-error "Aborting")))
             (choice-split (split-string choice " " t))
             (module (car choice-split))
             (repl (substring (cadr choice-split) 1 -1)))
        (setq command
              (intern-soft
               (format "+%s/open-%srepl" module
                       (if (string= repl "default")
                           ""
                         repl))))))
    (unless (commandp command)
      (error "Couldn't find a valid REPL for %s" major-mode))
    (when (+eval--ensure-in-repl-buffer command other-window-p)
      (when (bound-and-true-p evil-mode)
        (call-interactively #'evil-append-line))
      t)))

;;;###autoload
(defun +eval/open-repl-same-window (&optional arg)
  "Opens (or reopens) the REPL associated with the current major-mode and place
the cursor at the prompt.

If ARG (universal argument), prompt for a specific REPL to open."
  (interactive "P")
  (+eval-open-repl arg))

;;;###autoload
(defun +eval/open-repl-other-window (&optional arg)
  "Does `+eval/open-repl', but in a popup window.

If ARG (universal argument), prompt for a specific REPL to open."
  (interactive "P")
  (+eval-open-repl arg t))

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
