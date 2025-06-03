;;; lang/beancount/autoload/advice.el -*- lexical-binding: t; -*-

(defun +beancount--included-files (&optional recursive? context)
  "Return a list of included files in the current beancount buffer.

If RECURSIVE? is non-nil, included files will be read for each found include (as
will theirs, recursively)."
  (let ((nested? (eq recursive? 'nested))
        files)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^" (unless nested? "\\(?:;+# \\)?")
                        (rx "include" (+ " ") "\"" (group (+ (not "\"")))))
                nil t)
          (when-let* ((path (match-string-no-properties 1)))
            (setq path (expand-file-name path))
            (dolist (file (if (string-match "[[*?]" path)
                              (doom-glob path)
                            (when (file-readable-p path)
                              (list path))))
              (unless (and context
                           (let ((skip (doom-file-cookie file "skip")))
                             (or (eq skip t)
                                 (memq context (ensure-list skip)))))
                (cl-callf nconc files (list file))
                (when recursive?
                  (with-temp-buffer
                    (insert-file-contents file)
                    (let ((default-directory
                           (directory-file-name
                            (file-name-directory file))))
                      (cl-callf nconc files (+beancount--included-files 'nested context)))))))))
        (if nested?
            files
          (delete-dups files))))))

;;;###autoload
(defun +beancount--collect-unique-recursive (regexp n &optional context)
  (let ((results (make-hash-table :test 'equal))
        (buffer (or (buffer-base-buffer) (current-buffer)))
        (files (if (eq +beancount-files 'auto)
                   (+beancount--included-files t context)
                 +beancount-files)))
    (dolist (file (cons (buffer-file-name buffer) files))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (x (beancount-collect-pos-alist regexp n))
          (puthash (car x) `(:file ,file :point ,(cdr x))
                   results))))
    results))

(defvar +beancount--completion-cache nil)
;;;###autoload
(defun +beancount-completion-table (regexp n context &optional sort-fn)
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . ,(intern (format "beancount-%s" context)))
          (display-sort-function . identity))
      (make-local-variable '+beancount--completion-cache)
      (with-current-buffer (if (active-minibuffer-window)
                               (window-buffer (minibuffer-selected-window))
                             (current-buffer))
        (unless (assq context +beancount--completion-cache)
          (with-memoization (alist-get context +beancount--completion-cache)
            (+beancount--collect-unique-recursive regexp n context)))
        (complete-with-action
         action (sort (hash-table-keys
                       (alist-get context +beancount--completion-cache))
                      (or sort-fn #'string<))
         string pred)))))

(defun +beancount-account-completion-table (&optional sort-fn)
  (cl-callf2 assq-delete-all 'accounts +beancount--completion-cache)
  (+beancount-completion-table
   (concat "^" beancount-date-regexp " +open +\\(" beancount-account-regexp "\\)")
   1 'accounts sort-fn))

;;;###autoload
(defun +beancount-completion-at-point-a ()
  "Return the completion data relevant for the text at point."
  (save-excursion
    (save-match-data
      (let ((pos (point)))
        (beginning-of-line)
        (cond
         ;; non timestamped directive
         ((beancount-looking-at "[a-z]*" 0 pos)
          (list (match-beginning 0) (match-end 0)
                (mapcar (lambda (s) (concat s " ")) beancount-directive-names)))

         ;; poptag
         ((beancount-looking-at
           (concat "poptag\\s-+\\(\\(?:#[" beancount-tag-chars "]*\\)\\)") 1 pos)
          (list (match-beginning 1) (match-end 1)
                (beancount-collect-pushed-tags (point-min) (point))))

         ;; option
         ((beancount-looking-at
           (concat "^option\\s-+\\(\"[a-z_]*\\)") 1 pos)
          (list (match-beginning 1) (match-end 1)
                (mapcar (lambda (s) (concat "\"" s "\" ")) beancount-option-names)))

         ;; NEW: event values (scoped by name)
         ;; REVIEW: PR this upstream
         ((beancount-looking-at
           (concat beancount-date-regexp "\\s-+event\\s-+\"\\([^\"\n]+\\)\"\\s-+\"\\([^\"\n]*\\)")
           2 pos)
          (list (match-beginning 2) (match-end 2)
                (+beancount-completion-table
                 (concat "^" beancount-date-regexp
                         "\\s-+event"
                         "\\s-+\"" (regexp-quote (match-string-no-properties 1)) "\""
                         "\\s-+\"\\([^\"\n]+\\)\"")
                 1 'event-values)))

         ;; NEW: event names
         ;; REVIEW: PR this upstream
         ((beancount-looking-at
           (concat beancount-date-regexp "\\s-+event\\s-+\"\\([^\"\n]*\\)") 1 pos)
          (list (match-beginning 1) (match-end 1)
                (+beancount-completion-table
                 (concat "^" beancount-date-regexp
                         "\\s-+event"
                         "\\s-+\"\\([^\"\n]+\\)\"")
                 1 'events)))

         ;; NEW: transaction payees
         ;; REVIEW: PR this upstream (?)
         ((beancount-looking-at
           (concat beancount-date-regexp "\\s-+" beancount-flag-regexp "\\s-+\"\\([^\"\n]*\\)")
           1 pos)
          (list (match-beginning 1) (match-end 1)
                (+beancount-completion-table
                 (concat "^" beancount-date-regexp
                         "\\s-+" beancount-flag-regexp
                         "\\s-+\"\\([a-zA-Z0-9][^\"\n]+\\)\"")
                 1 'payees)))

         ;; timestamped directive
         ((beancount-looking-at
           (concat beancount-date-regexp "\\s-+\\([[:alpha:]]*\\)") 1 pos)
          (list (match-beginning 1) (match-end 1)
                (mapcar (lambda (s) (concat s " ")) beancount-timestamped-directive-names)))

         ;; timestamped directives followed by account
         ((beancount-looking-at
           (concat "^" beancount-date-regexp
                   "\\s-+" (regexp-opt beancount-account-directive-names)
                   "\\s-+\\([" beancount-account-chars "]*\\)")
           1 pos)
          (list (match-beginning 1) (match-end 1) (+beancount-account-completion-table)))

         ;; budget directive followed by an account
         ((beancount-looking-at
           (concat "^" beancount-date-regexp "\\s-+"
                   "custom\\s-+\"budget\"\\s-+\\([" beancount-account-chars "]*\\)")
           1 pos)
          (list (match-beginning 1) (match-end 1) (+beancount-account-completion-table)))

         ;; pad directive followed by two accounts
         ((beancount-looking-at
           (concat "^" beancount-date-regexp
                   "\\s-+" (regexp-opt '("pad"))
                   "\\s-+\\([" beancount-account-chars "]*\\)"
                   "\\s-+\\([" beancount-account-chars "]*\\)") 2 pos)
          (list (match-beginning 2) (match-end 2) (+beancount-account-completion-table)))

         ;; posting
         ((and (beancount-looking-at
                (concat "[ \t]+\\([" beancount-account-chars "]*\\)") 1 pos)
               ;; Do not force the account name to start with a
               ;; capital, so that it is possible to use substring
               ;; completion and we can rely on completion to fix
               ;; capitalization thanks to completion-ignore-case.
               (beancount-inside-transaction-p))
          (list (match-beginning 1) (match-end 1) (+beancount-account-completion-table)))

         ;; tags & links
         ;; REVIEW: The upstream implementation only completes tags/links at
         ;;   indentation and not at the end of transaction heading, where
         ;;   they're typically used. This fixes that.
         ((progn
            (goto-char pos)
            (and (re-search-backward "[ \t]" (pos-bol) t)
                 (looking-at (concat "[ \t]+\\(\\([#^]\\)[" beancount-tag-chars "]*\\)"))
                 (>= pos (match-beginning 1))
                 (<= pos (match-end 1))))
          (list (match-beginning 1) (match-end 1)
                (+beancount-completion-table
                 (concat " \\(" (match-string-no-properties 2) "[" beancount-tag-chars "]+\\)")
                 1 (if (equal (match-string-no-properties 2) "#") 'tags 'links))))

         ((progn
            (goto-char pos)
            (and (looking-back (concat "\\s-" beancount-number-regexp
                                       "\\s-+\\(\\(?:" beancount-currency-regexp
                                       "\\)?\\)")
                               (pos-bol))
                 (>= pos (match-beginning 1))
                 (<= pos (match-end 1))))
          (list (match-beginning 1) (match-end 1)
                (+beancount-completion-table
                 (concat "^\\(?:" beancount-date-regexp
                         "\\s-+commodity\\s-+\\|"
                         "option\\s-+\"operating_currency\"\\s-+\""
                         "\\)\\(" beancount-currency-regexp "\\)")
                 1 'commodities))))))))

;;;###autoload
(defun +beancount-get-account-names-a (&rest _)
  "Crawl `+beancount-files' for account names."
  (unless beancount-accounts
    (setq beancount-accounts
          (hash-table-keys
           (+beancount--collect-unique-recursive
            (concat "^" beancount-date-regexp " +open +\\(" beancount-account-regexp "\\)")
            1 'accounts))))
  beancount-accounts)

;;;###autoload
(defun +beancount--flymake-bean-check--run-a (report-fn &rest _ignored)
  (unless (executable-find flymake-bean-check-executable)
    (error "The executable %s doesn't exist. See `flymake-bean-check-executable'"
           flymake-bean-check-executable))
  (when (and flymake-bean-check-process
             (process-live-p flymake-bean-check-process))
    (kill-process flymake-bean-check-process))
  (let* ((source (current-buffer))
         (buffer (generate-new-buffer "*flymake-bean-check*"))
         (cache-file (flymake-bean-check-cache-filename (buffer-file-name))))
    (setq flymake-bean-check-process
          (make-process :buffer buffer
                        :name "flymake-bean-check"
                        :noquery t
                        :connection-type 'pipe
                        :command (list flymake-bean-check-executable
                                       "/dev/stdin"
                                       "--cache-filename" cache-file)
                        :sentinel
                        (lambda (proc _event)
                          (when (memq (process-status proc) '(exit signal))
                            (unwind-protect
                                (with-current-buffer buffer
                                  (goto-char (point-min))
                                  (let (result)
                                    (while (re-search-forward flymake-bean-check-location-regexp
                                                              nil t)
                                      (pcase-let*
                                          ((message (match-string 2))
                                           (`(,begin . ,end) (flymake-diag-region
                                                              source
                                                              (string-to-number (match-string 1)))))
                                        (push (flymake-make-diagnostic source begin end
                                                                       :error message)
                                              result)))
                                    (funcall report-fn (nreverse result))))
                              (kill-buffer buffer))))))
    (process-send-string
     flymake-bean-check-process
     (save-restriction
       (widen)
       (with-temp-buffer
         (save-excursion (insert-buffer-substring-no-properties source))
         (save-excursion
           (while (re-search-forward "^;+# " nil t)
             (replace-match "" t t)))
         (while (re-search-forward
                 (rx bol
                     (or (seq (= 4 num) "-" (= 2 num) "-" (= 2 num) (+ " ")
                              "document" (+ " ")
                              (+ (or alnum ":" "_" "-")))
                         "include"
                         (seq "option" (+ " ") "\"documents\""))
                     (+ " ") "\""
                     (group (+ (not "\""))))
                 nil t)
           (unless (file-name-absolute-p (match-string-no-properties 1))
             (replace-match (expand-file-name
                             (match-string-no-properties 1))
                            t t nil 1)))
         (buffer-substring-no-properties (point-min) (point-max)))))
    (process-send-eof flymake-bean-check-process)))

;;; advice.el ends here
