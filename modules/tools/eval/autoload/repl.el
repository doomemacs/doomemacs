;;; tools/eval/autoload/repl.el -*- lexical-binding: t; -*-

(defvar +eval-repl-buffers (make-hash-table :test 'equal)
  "The buffer of the last open repl.")

(defvar-local +eval-repl-plist nil)

(define-minor-mode +eval-repl-mode
  "A minor mode for REPL buffers.")

(defun +eval--ensure-in-repl-buffer (&optional fn plist displayfn)
  (maphash (lambda (key buffer)
             (unless (buffer-live-p buffer)
               (remhash key +eval-repl-buffers)))
           +eval-repl-buffers)
  (let* ((project-root (doom-project-root))
         (key (cons major-mode project-root))
         (buffer (gethash key +eval-repl-buffers)))
    (cl-check-type buffer (or buffer null))
    (unless (or (eq buffer (current-buffer))
                (null fn))
      (setq buffer
            (funcall (or displayfn #'get-buffer-create)
                     (if (buffer-live-p buffer)
                         buffer
                       (setq buffer
                             (save-window-excursion
                               (if (commandp fn)
                                   (call-interactively fn)
                                 (funcall fn))))
                       (cond ((null buffer)
                              (error "REPL handler %S couldn't open the REPL buffer" fn))
                             ((not (bufferp buffer))
                              (error "REPL handler %S failed to return a buffer" fn)))
                       (with-current-buffer buffer
                         (when plist
                           (setq +eval-repl-plist plist))
                         (+eval-repl-mode +1))
                       (puthash key buffer +eval-repl-buffers)
                       buffer))))
    (when (bufferp buffer)
      (with-current-buffer buffer
        (unless (or (derived-mode-p 'term-mode)
                    (eq (current-local-map) (bound-and-true-p term-raw-map)))
          (goto-char (if (and (derived-mode-p 'comint-mode)
                              (cdr comint-last-prompt))
                         (cdr comint-last-prompt)
                       (point-max)))))
      buffer)))

(defun +eval-open-repl (prompt-p &optional displayfn)
  (cl-destructuring-bind (mode fn . plist)
      (or (assq major-mode +eval-repls)
          (list))
    (when (or (not fn) prompt-p)
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
        (setq fn
              (intern-soft
               (format "+%s/open-%srepl" module
                       (if (string= repl "default")
                           ""
                         repl))))))
    (let ((region (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end)))))
      (unless (commandp fn)
        (error "Couldn't find a valid REPL for %s" major-mode))
      (with-current-buffer (+eval--ensure-in-repl-buffer fn plist displayfn)
        (when (bound-and-true-p evil-mode)
          (call-interactively #'evil-append-line))
        (when region
          (insert region))
        t))))


;;
;;; Commands

;;;###autoload
(defun +eval/open-repl-same-window (&optional arg)
  "Opens (or reopens) the REPL associated with the current major-mode and place
the cursor at the prompt.

If ARG (universal argument), prompt for a specific REPL to open."
  (interactive "P")
  (+eval-open-repl arg #'switch-to-buffer))

;;;###autoload
(defun +eval/open-repl-other-window (&optional arg)
  "Does `+eval/open-repl', but in a popup window.

If ARG (universal argument), prompt for a specific REPL to open."
  (interactive "P")
  (+eval-open-repl arg #'pop-to-buffer))

;;;###autoload
(defun +eval/send-region-to-repl (beg end &optional inhibit-auto-execute-p)
  "Execute the selected region in the REPL.
Opens a REPL if one isn't already open. If AUTO-EXECUTE-P, then execute it
immediately after."
  (interactive "rP")
  (let ((selection (buffer-substring-no-properties beg end))
        (buffer (+eval--ensure-in-repl-buffer)))
    (unless buffer
      (error "No REPL open"))
    (with-selected-window (get-buffer-window buffer)
      (when (bound-and-true-p evil-local-mode)
        (call-interactively #'evil-append-line))
      (insert selection)
      (unless inhibit-auto-execute-p
        ;; `comint-send-input' isn't enough because some REPLs may not use
        ;; comint, so just emulate the keypress.
        (execute-kbd-macro (kbd "RET"))))))
