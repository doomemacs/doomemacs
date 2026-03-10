;;; tools/eval/autoload/repl.el -*- lexical-binding: t; -*-

(defvar evil-move-cursor-back)


;;
;;; Variables

(defvar +eval-repl-buffers (make-hash-table :test 'equal)
  "The buffer of the last open repl.")

(defvar +eval-repl-plist nil)


;;
;;; Library

;;;###autoload
(defun +eval-current-repl-buffer (&optional mode)
  "Return the last active REPL buffer associated with this buffer's major mode.

Returns nil if none is known. If multiple are known, it returns the last
accessed buffer."
  (when-let* ((project-root (doom-project-root))
              (key (cons (or mode major-mode) project-root))
              (buffer (gethash key +eval-repl-buffers)))
    (and (bufferp buffer)
         (buffer-live-p buffer)
         (buffer-local-value '+eval-repl-plist buffer)
         buffer)))

;;;###autoload
(defun +eval-repl-select (prompt)
  "Prompt the user to select a REPL.

Prompt with PROMPT, which should be a string ending with a colon and a space.
Scans `+eval-repl-handler-alist' and all known symbols that look like
*/open-*repl and returns (NAME COMMAND), where NAME is a string label and
COMMAND is a symbol for an interactive function."
  (let* ((knowns
          (mapcar
           (lambda (spec)
             (unless (fboundp (car spec))
               (error "Given string/symbol is not a major mode: %s" (car spec)))
             (list (string-join
                    (split-string
                     (capitalize (string-remove-suffix "-mode" (symbol-name (car spec))))
                     "-")
                    " ")
                   (cadr spec)))
           +eval-repl-handler-alist))
         (founds
          (mapcar
           (lambda (spec)
             (list (string-join (split-string (capitalize (cadr spec)) "-") " ")
                   (car spec)))
           (cl-loop for sym being the symbols
                    for sym-name = (symbol-name sym)
                    if (string-match "^\\(?:\\+\\)?\\([^/]+\\)/open-\\(?:\\(.+\\)-\\)?repl$" sym-name)
                    collect (list sym (match-string-no-properties 1 sym-name)))))
         (repls (cl-delete-duplicates (append knowns founds) :test #'equal)))
    (or (assoc (or (completing-read (or prompt "Open a REPL for: ")
                                    (mapcar #'car repls))
                   (user-error "aborting"))
               repls)
        (error "couldn't find a valid repl for %s" major-mode))))

(defun +eval--repl-open (spec &optional displayfn input)
  "open a repl via the given displayfn. if prompt-p, the user will be
prompted for a repl choice, even if the major mode they're in
already has a known one."
  (maphash (lambda (key buffer)
             (unless (buffer-live-p buffer)
               (remhash key +eval-repl-buffers)))
           +eval-repl-buffers)
  (pcase-let ((`(_ ,fn . ,plist) spec))
    (unless (commandp fn)
      (error "couldn't find a valid REPL handler for %s" major-mode))
    (let* ((project-root (doom-project-root))
           (key (cons major-mode project-root))
           buffer)
      (setq buffer
            (funcall (or displayfn #'get-buffer-create)
                     (if (buffer-live-p buffer)
                         buffer
                       (setq buffer
                             (save-window-excursion
                               (if (commandp fn)
                                   (call-interactively fn)
                                 (funcall fn))))
                       (unless buffer
                         (error "REPL handler %S couldn't open the REPL buffer" fn))
                       (unless (bufferp buffer)
                         (error "REPL handler %S failed to return a buffer" fn))
                       (with-current-buffer buffer
                         (setq-local +eval-repl-plist (append (list :repl t) plist)))
                       (puthash key buffer +eval-repl-buffers)
                       buffer)))
      (when (bufferp buffer)
        (with-current-buffer buffer
          (unless (or (derived-mode-p 'term-mode)
                      (eq (current-local-map) (bound-and-true-p term-raw-map)))
            (goto-char (if (and (derived-mode-p 'comint-mode)
                                (cdr comint-last-prompt))
                           (cdr comint-last-prompt)
                         (point-max))))
          (when (bound-and-true-p evil-local-mode)
            (call-interactively #'evil-append-line))
          (when input
            (insert input))
          t)))))

(defun +eval--repl-sender-for (mode &optional beg end)
  (when-let*
      ((plist (cdr (alist-get mode +eval-repl-handler-alist)))
       (fn (or (plist-get plist (if (and beg end) :send-region :send-buffer))
               (unless (and beg end) (plist-get plist :send-region)))))
    (if (and beg end)
        (lambda () (funcall fn beg end))
      fn)))


;;
;;; Eval handlers

;;;###autoload
(defun +eval-with-repl-fn (beg end &optional type)
  "Evaluate the region between BEG and END (inclusive) in an open REPL.

If no REPL is open, do nothing. TYPE can be `buffer' or `region' to determine
what sender to use, if one's been registered with the repl for the current major
mode."
  (when-let* ((buf (+eval-current-repl-buffer))
              ((get-buffer-window buf)))
    (if-let* ((fn (if (eq type 'buffer)
                      (+eval--repl-sender-for major-mode)
                    (+eval--repl-sender-for major-mode beg end))))
        (funcall fn)
      ;; Manually feed selection line-by-line if this repl has no
      ;; :send-buffer/:send-region properties for its `set-repl-handler!'
      ;; handler. This is a last resort and may be rife with edge cases.
      (let* ((region (buffer-substring-no-properties beg end))
             (region
              (with-temp-buffer
                (save-excursion (insert region))
                (when (> (skip-chars-forward "\n") 0)
                  (delete-region (point-min) (point)))
                (indent-rigidly (point-min) (point-max) (- (current-indentation)))
                (buffer-string))))
        (with-selected-window (get-buffer-window buf)
          (with-current-buffer buf
            (goto-char (point-max))
            (dolist (line (split-string region "\n"))
              (insert line)
              ;; HACK: Can't use `comint-send-input' b/c there's no guarantee
              ;;   the current REPL uses comint. Even if it did, no telling if
              ;;   they have their own `comint-send-input' wrapper, so to be
              ;;   safe, I simply emulate the keypress.
              (if (bound-and-true-p evil-local-mode)
                  (let (evil-move-cursor-back)
                    (evil-save-state
                      (evil-append-line 1)
                      (call-interactively (doom-lookup-key (kbd "RET")))))
                (call-interactively (doom-lookup-key (kbd "RET"))))))))
      t)))


;;
;;; Commands

;;;###autoload
(defun +eval/open-repl-same-window (&optional spec input)
  "Opens (or reopens) the REPL associated with the current major-mode and place
the cursor at the prompt.

If ARG (universal argument), prompt for a specific REPL to open."
  (interactive
   (list (or (unless current-prefix-arg
               (assq major-mode +eval-repl-handler-alist))
             (+eval-repl-select "Open REPL in this window: "))
         (doom-region)))
  (+eval--repl-open spec #'switch-to-buffer input))

;;;###autoload
(defun +eval/open-repl-other-window (&optional spec input)
  "Does `+eval/open-repl', but in a popup window.

If ARG (universal argument), prompt for a specific REPL to open."
  (interactive
   (list (or (unless current-prefix-arg
               (assq major-mode +eval-repl-handler-alist))
             (+eval-repl-select "Open REPL in popup: "))
         (doom-region)))
  (+eval--repl-open spec #'pop-to-buffer input))

;;;###autoload
(defun +eval/buffer-or-region-in-repl (&optional beg end buffer?)
  "Execute the selected region or whole buffer in the REPL."
  (interactive "rP")
  (unless (+eval-current-repl-buffer)
    (call-interactively #'+eval/open-repl-other-window))
  (let* ((region? (and (not buffer?) (doom-region-active-p)))
         (type (if region? 'region 'buffer))
         (beg (if region? beg (point-min)))
         (end (if region? end (point-max))))
    (+eval-with-repl-fn beg end type)))

;;; repl.el ends here
