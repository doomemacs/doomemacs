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

(defun +eval-repl-known-repls ()
  "Yield the available repl functions as a list of symbols."
  (cl-delete-duplicates
   (mapcar (lambda! ((mode fn &rest _)) (list mode fn))
           +eval-repls)))

(defun +doom-pretty-mode-name (mode)
  "Convert a mode name into a variant nicer for human eyes."
  (let ((mode (if (symbolp mode) (symbol-name mode) mode)))
    (if (not (string-match "^\\([a-z-]+\\)-mode$" mode))
        (error "Given string/symbol is not a major mode: %s" mode)
      (string-join (split-string (capitalize (match-string-no-properties 1 mode))
                                 "-")
                   " "))))

(defun +eval-repl-found-repls ()
  "Search the interned symbol list for functions that looks like
repl openers."
  (cl-loop for sym being the symbols
           for sym-name = (symbol-name sym)
           if (string-match "^\\(?:\\+\\)?\\([^/]+\\)/open-\\(?:\\(.+\\)-\\)?repl$" sym-name)
           collect
           sym))

(defun +eval-pretty-mode-name-from-fn (fn)
  "Given a symbol name of a repl-opening function, extract a
human-readable variant of its associated major mode name."
  (let ((name (symbol-name fn)))
    (if (not (string-match "^\\(?:\\+\\)?\\([^/]+\\)/open-\\(?:\\(.+\\)-\\)?repl$" name))
        (error "Given symbol is not a repl function: %s" name)
      (string-join (split-string (capitalize (match-string-no-properties 1 name))
                                 "-")
                   " "))))

(defun +eval-repl-prompt ()
  "Prompt the user for the choice of a repl to open."
  (let* ((knowns (mapcar (lambda! ((mode fn)) (list (+doom-pretty-mode-name mode) fn))
                         (+eval-repl-known-repls)))
         (founds (mapcar (lambda (fn) (list (+eval-pretty-mode-name-from-fn fn) fn))
                         (+eval-repl-found-repls)))
         (repls (cl-delete-duplicates (append knowns founds) :test #'equal))
         (names (mapcar #'car repls))
         (choice (or (completing-read "Open a REPL for: " names)
                     (user-error "Aborting"))))
    (cadr (assoc choice repls))))

(defun +eval-repl-from-major-mode ()
  "Fetch the repl associated with the current major mode, if there
is one."
  (pcase-let ((`(_ ,fn . ,plist) (assq major-mode +eval-repls)))
    (list fn plist)))

(defun +eval-open-repl (prompt-p &optional displayfn)
  "Open a repl via the given DISPLAYFN. If PROMPT-P, the user will be
prompted for a repl choice, even if the major mode they're in
already has a known one."
  (pcase-let* ((`(,fn ,plist) (+eval-repl-from-major-mode))
               (fn (if (or prompt-p (not fn)) (+eval-repl-prompt) fn))
               (region (when (use-region-p)
                         (buffer-substring-no-properties (region-beginning)
                                                         (region-end)))))
    (unless (commandp fn)
      (error "Couldn't find a valid REPL for %s" major-mode))
    (with-current-buffer (+eval--ensure-in-repl-buffer fn plist displayfn)
      (when (bound-and-true-p evil-mode)
        (call-interactively #'evil-append-line))
      (when region
        (insert region))
      t)))

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
    (let ((origin-window (selected-window))
          (selection
           (with-temp-buffer
             (insert selection)
             (goto-char (point-min))
             (when (> (skip-chars-forward "\n") 0)
               (delete-region (point-min) (point)))
             (indent-rigidly (point) (point-max)
                             (- (skip-chars-forward " \t")))
             (string-trim-right (buffer-string)))))
      (with-selected-window (get-buffer-window buffer)
        (with-current-buffer buffer
          (dolist (line (split-string selection "\n"))
            (insert line)
            (if inhibit-auto-execute-p
                (insert "\n")
              ;; Can't use `comint-send-input' b/c there's no guarantee the
              ;; current REPL uses comint. Even if it did, no telling if they
              ;; have their own `comint-send-input' wrapper, so to be safe, I
              ;; simply emulate the keypress.
              (call-interactively
               (if (bound-and-true-p evil-local-mode)
                   (evil-save-state
                     (evil-insert-state)
                     (doom-lookup-key (kbd "RET")))
                 (doom-lookup-key (kbd "RET")))))
            (sit-for 0.001)
            (redisplay 'force)))
        (when (and (eq origin-window (selected-window))
                   (bound-and-true-p evil-local-mode))
          (call-interactively #'evil-append-line))))))
