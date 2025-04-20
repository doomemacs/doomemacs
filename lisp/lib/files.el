;;; lisp/lib/files.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun doom-files--build-checks (spec &optional directory)
  "Converts a simple nested series of or/and forms into a series of
`file-exists-p' checks.

For example

  (doom-files--build-checks
    '(or A (and B C))
    \"~\")

Returns (not precisely, but effectively):

  '(let* ((_directory \"~\")
          (A (expand-file-name A _directory))
          (B (expand-file-name B _directory))
          (C (expand-file-name C _directory)))
     (or (and (file-exists-p A) A)
         (and (if (file-exists-p B) B)
              (if (file-exists-p C) C))))

This is used by `file-exists-p!' and `project-file-exists-p!'."
  (declare (pure t) (side-effect-free t))
  (if (and (listp spec)
           (memq (car spec) '(or and)))
      (cons (car spec)
            (cl-loop for it in (cdr spec)
                     collect (doom-files--build-checks it directory)))
    (let ((filevar (make-symbol "file")))
      `(let ((,filevar ,spec))
         (and (stringp ,filevar)
              ,(if directory
                   `(let ((default-directory ,directory))
                      (file-exists-p ,filevar))
                 `(file-exists-p ,filevar))
              ,filevar)))))

;;;###autoload
(defun doom-path (&rest segments)
  "Return an path expanded after concatenating SEGMENTS with path separators.

Ignores `nil' elements in SEGMENTS, and is intended as a fast compromise between
`expand-file-name' (slow, but accurate), `file-name-concat' (fast, but
inaccurate)."
  (declare (side-effect-free t))
  ;; PERF: An empty `file-name-handler-alist' = faster `expand-file-name'.
  (let (file-name-handler-alist)
    (expand-file-name
     ;; PERF: avoid the overhead of `apply' in the trivial case. This function
     ;;   is used a lot, so every bit counts.
     (if (cdr segments)
         (apply #'file-name-concat segments)
       (car segments)))))

;;;###autoload
(defun doom-glob (&rest segments)
  "Return file list matching the glob created by joining SEGMENTS.

The returned file paths will be relative to `default-directory', unless SEGMENTS
concatenate into an absolute path.

Returns nil if no matches exist.
Ignores `nil' elements in SEGMENTS.
If the glob ends in a slash, only returns matching directories."
  (declare (side-effect-free t))
  (let* (case-fold-search
         file-name-handler-alist
         (path (apply #'file-name-concat segments)))
    (if (string-suffix-p "/" path)
        (cl-loop for file in (file-expand-wildcards (substring path 0 -1))
                 if (file-directory-p file)
                 collect file)
      (file-expand-wildcards path))))

;;;###autoload
(define-obsolete-function-alias 'doom-dir 'doom-path "3.0.0")

;;;###autoload
(cl-defun doom-files-in
    (paths &rest rest
           &key
           filter
           map
           (full t)
           (follow-symlinks t)
           (type 'files)
           (relative-to (unless full default-directory))
           (depth 99999)
           (mindepth 0)
           (match "/[^._][^/]+"))
  "Return a list of files/directories in PATHS (one string or a list of them).

FILTER is a function or symbol that takes one argument (the path). If it returns
non-nil, the entry will be excluded.

MAP is a function or symbol which will be used to transform each entry in the
results.

TYPE determines what kind of path will be included in the results. This can be t
(files and folders), 'files or 'dirs.

By default, this function returns paths relative to PATH-OR-PATHS if it is a
single path. If it a list of paths, this function returns absolute paths.
Otherwise, by setting RELATIVE-TO to a path, the results will be transformed to
be relative to it.

The search recurses up to DEPTH and no further. DEPTH is an integer.

MATCH is a string regexp. Only entries that match it will be included."
  (let (result)
    (dolist (file (mapcan (doom-rpartial #'doom-glob "*") (ensure-list paths)))
      (cond ((file-directory-p file)
             (cl-callf append result
               (and (memq type '(t dirs))
                    (string-match-p match file)
                    (not (and filter (funcall filter file)))
                    (not (and (file-symlink-p file)
                              (not follow-symlinks)))
                    (<= mindepth 0)
                    (list (if relative-to
                              (file-relative-name file relative-to)
                            file)))
               (and (>= depth 1)
                    (apply #'doom-files-in file
                           (append (list :mindepth (1- mindepth)
                                         :depth (1- depth)
                                         :relative-to relative-to
                                         :map nil)
                                   rest)))))
            ((and (memq type '(t files))
                  (string-match-p match file)
                  (not (and filter (funcall filter file)))
                  (<= mindepth 0))
             (push (if relative-to
                       (file-relative-name file relative-to)
                     file)
                   result))))
    (if map
        (mapcar map result)
      result)))

;;;###autoload
(defun doom-file-cookie (file &optional cookie null-value)
  "Returns the evaluated result of FORM in a ;;;###COOKIE FORM at the top of
FILE.

If COOKIE doesn't exist, or cookie isn't within the first 256 bytes of FILE,
return NULL-VALUE."
  (unless (file-exists-p file)
    (signal 'file-missing file))
  (unless (file-readable-p file)
    (error "%S is unreadable" file))
  (with-temp-buffer
    (insert-file-contents file nil 0 256)
    (if (re-search-forward (format "^;;;###%s " (regexp-quote (or cookie "if")))
                           nil t)
        (sexp-at-point)
      null-value)))

;;;###autoload
(defun doom-file-cookie-p (file &optional cookie null-value)
  "Returns the evaluated result of FORM in a ;;;###COOKIE FORM at the top of
FILE.

If COOKIE doesn't exist, or cookie isn't within the first 256 bytes of FILE,
return NULL-VALUE."
  (let ((sexp (doom-file-cookie file cookie null-value)))
    (if (equal sexp null-value)
        null-value
      (with-temp-buffer
        (with-doom-module (doom-module-from-path file)
          (let ((load-file-name file))
            (eval (doom-file-cookie file cookie null-value) t)))))))

;;;###autoload
(defmacro file-exists-p! (files &optional directory)
  "Returns non-nil if the FILES in DIRECTORY all exist.

DIRECTORY is a path; defaults to `default-directory'.

Returns the last file found to meet the rules set by FILES, which can be a
single file or nested compound statement of `and' and `or' statements."
  `(let ((p ,(doom-files--build-checks files directory)))
     (and p (expand-file-name p ,directory))))

;;;###autoload
(defun doom-file-size (file &optional dir)
  "Returns the size of FILE (in DIR) in bytes."
  (let ((file (expand-file-name file dir)))
    (unless (file-exists-p file)
      (error "Couldn't find file %S" file))
    (unless (file-readable-p file)
      (error "File %S is unreadable; can't acquire its filesize"
             file))
    (nth 7 (file-attributes file))))

(defvar w32-get-true-file-attributes)
;;;###autoload
(defun doom-directory-size (dir)
  "Returns the size of FILE (in DIR) in kilobytes."
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (if (executable-find "du")
      (/ (string-to-number (cdr (doom-call-process "du" "-sb" dir)))
         1024.0)
    ;; REVIEW This is slow and terribly inaccurate, but it's something
    (let ((w32-get-true-file-attributes t)
          (file-name-handler-alist dir)
          (max-lisp-eval-depth 5000)
          (sum 0.0))
      (dolist (attrs (directory-files-and-attributes dir nil nil t) sum)
        (unless (member (car attrs) '("." ".."))
          (cl-incf
           sum (if (eq (nth 1 attrs) t) ; is directory
                   (doom-directory-size (expand-file-name (car attrs) dir))
                 (/ (nth 8 attrs) 1024.0))))))))


;;
;;; File read/write

(defmacro doom--with-prepared-file-buffer (file coding mode &rest body)
  "Create a temp buffer and prepare it for file IO in BODY."
  (declare (indent 3))
  (let ((nmask (make-symbol "new-mask"))
        (omask (make-symbol "old-mask")))
    `(let* ((,nmask ,mode)
            (,omask (if ,nmask (default-file-modes))))
       (unwind-protect
           (with-temp-buffer
             (if ,nmask (set-default-file-modes ,nmask))
             (let* ((buffer-file-name (doom-path ,file))
                    (coding-system-for-read  (or ,coding 'binary))
                    (coding-system-for-write (or coding-system-for-write coding-system-for-read 'binary)))
               (when (eq coding-system-for-read 'binary)
                 (set-buffer-multibyte nil)
                 (setq-local buffer-file-coding-system 'binary))
               ,@body))
         (if ,nmask (set-default-file-modes ,omask))))))

;;;###autoload
(cl-defun doom-file-read
    (file &key
          (by 'buffer-string)
          (coding (or coding-system-for-read 'utf-8))
          noerror
          beg end)
  "Read FILE and return its contents.

Set BY to change how its contents are consumed. It accepts any function, to be
called with no arguments and expected to return the contents as any arbitrary
data. By default, BY is set to `buffer-string'. Otherwise, BY recognizes these
special values:

'insert      -- insert FILE's contents into the current buffer before point.
'read        -- read the first form in FILE and return it as a single S-exp.
'read*       -- read all forms in FILE and return it as a list of S-exps.
'(read . N)  -- read the first N (an integer) S-exps in FILE.

CODING dictates the encoding of the buffer. This defaults to `utf-8'. If set to
nil, `binary' is used.

If NOERROR is non-nil, don't throw an error if FILE doesn't exist. This will
still throw an error if FILE is unreadable, however.

If BEG and/or END are integers, only that region will be read from FILE."
  (when (or (not noerror)
            (file-exists-p file))
    (let ((old-buffer (current-buffer)))
      (doom--with-prepared-file-buffer file coding nil
        (if (not (eq coding-system-for-read 'binary))
            (insert-file-contents buffer-file-name nil beg end)
          (insert-file-contents-literally buffer-file-name nil beg end))
        (pcase by
          ('insert
           (if (fboundp 'insert-into-buffer)
               (insert-into-buffer old-buffer)
             ;; DEPRECATED: Remove fallback when 27.x support is dropped.
             (let ((input (buffer-substring-no-properties (point-min) (point-max))))
               (with-current-buffer old-buffer
                 (insert input))))
           t)
          ('buffer-string
           (buffer-substring-no-properties (point-min) (point-max)))
          ('read
           (condition-case _ (read (current-buffer)) (end-of-file)))
          ('(read . ,i)
           (let (forms)
             (condition-case _
                 (dotimes (_ i) (push (read (current-buffer)) forms))
               (end-of-file))
             (nreverse forms)))
          ('read*
           (let (forms)
             (condition-case _
                 (while t (push (read (current-buffer)) forms))
               (end-of-file))
             (nreverse forms)))
          (fn (funcall fn)))))))

;;;###autoload
(cl-defun doom-file-write
    (file contents
          &key
          append
          (coding 'utf-8)  ; default: `utf-8'
          mode             ; default: `default-file-modes' (#o755)
          (mkdir    'parents)
          (insertfn #'insert)
          (printfn  #'prin1))
  "Write CONTENTS (a string or list of forms) to FILE (a string path).

If CONTENTS is list of forms. Any literal strings in the list are inserted
verbatim, as text followed by a newline, with `insert'. Sexps are inserted with
`prin1'. BY is the function to use to emit

MODE dictates the permissions of created file and directories. MODE is either an
integer or a cons cell whose car is the mode for files and cdr the mode for
directories. If FILE already exists, its permissions will be changed. The
permissions of existing directories will never be changed.

CODING dictates the encoding to read/write with (see `coding-system-for-write').
This defaults to `utf-8'. If set to nil, `binary' is used.

APPEND dictates where CONTENTS will be written. If neither is set,
the file will be overwritten. If both are, the contents will be written to both
ends. Set either APPEND or PREPEND to `noerror' to silently ignore read errors."
  (let ((mode (ensure-list mode))
        (contents (ensure-list contents))
        datum)
    (doom--with-prepared-file-buffer file coding (car mode)
      (while (setq datum (pop contents))
        (cond ((stringp datum)
               (funcall
                insertfn (if (or (string-suffix-p "\n" datum)
                                 (stringp (cadr contents)))
                             datum
                           (concat datum "\n"))))
              ((bufferp datum)
               (insert-buffer-substring datum))
              ((let ((standard-output (current-buffer))
                     (print-quoted t)
                     (print-level nil)
                     (print-length nil)
                     ;; Escape special chars to avoid any shenanigans
                     (print-escape-newlines t)
                     (print-escape-control-characters t)
                     (print-escape-nonascii t)
                     (print-escape-multibyte t))
                 (funcall printfn datum)))))
      (let (write-region-annotate-functions
            write-region-post-annotation-function)
        (when mkdir
          (with-file-modes (or (cdr mode) (default-file-modes))
            (make-directory (file-name-directory buffer-file-name)
                            (eq mkdir 'parents))))
        (write-region nil nil buffer-file-name append :silent))
      buffer-file-name)))

;;;###autoload
(defmacro with-file-contents! (file &rest body)
  "Create a temporary buffer with FILE's contents and execute BODY in it.

The point is at the beginning of the buffer afterwards.

A convenience macro to express the common `with-temp-buffer' +
`insert-file-contents' idiom more succinctly, enforce `utf-8', and perform some
optimizations for `binary' IO."
  (declare (indent 1))
  `(doom--with-prepared-file-buffer ,file (or coding-system-for-read 'utf-8) nil
     (doom-file-read buffer-file-name :by 'insert :coding coding-system-for-read)
     (goto-char (point-min))
     ,@body))

;;;###autoload
(defmacro with-file! (file &rest body)
  "Evaluate BODY in a temp buffer, then write its contents to FILE.

Unlike `with-temp-file', this uses the `utf-8' encoding by default and performs
some optimizations for `binary' IO."
  (declare (indent 1))
  `(doom--with-prepared-file-buffer ,file (or coding-system-for-read 'utf-8) nil
     (prog1 (progn ,@body)
       (doom-file-write buffer-file-name (current-buffer)
                        :coding (or coding-system-for-write 'utf-8)))))


;;
;;; Helpers

(defun doom-files--update-refs (&rest files)
  "Ensure FILES are updated in `recentf', `magit' and `save-place'."
  (let (toplevels)
    (dolist (file files)
      (when (featurep 'vc)
        (vc-file-clearprops file)
        (when-let (buffer (get-file-buffer file))
          (with-current-buffer buffer
            (vc-refresh-state))))
      (when (featurep 'magit)
        (when-let (default-directory (magit-toplevel (file-name-directory file)))
          (cl-pushnew default-directory toplevels)))
      (unless (file-readable-p file)
        (when (bound-and-true-p recentf-mode)
          (recentf-remove-if-non-kept file))
        (when (and (bound-and-true-p projectile-mode)
                   (doom-project-p)
                   (projectile-file-cached-p file (doom-project-root)))
          (projectile-purge-file-from-cache file))))
    (dolist (default-directory toplevels)
      (magit-refresh))
    (when (bound-and-true-p save-place-mode)
      (save-place-forget-unreadable-files))))


;;
;;; Commands

;;;###autoload
(defun doom/delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (and path (abbreviate-file-name path))))
    (unless path
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          ;; Ensures that windows displaying this buffer will be switched to
          ;; real buffers (`doom-real-buffer-p')
          (doom/kill-this-buffer-in-all-windows buf t)
          (doom-files--update-refs path)
          (message "Deleted %S" short-path))))))

;;;###autoload
(defun doom/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH then open NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (find-file new-path)
    (doom-files--update-refs old-path new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))

;;;###autoload
(defun doom/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (file-name-nondirectory old-path))))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (doom-files--update-refs old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

(defun doom--sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let* ((user (file-remote-p file 'user)))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

;;;###autoload
(defun doom/sudo-find-file (file &optional arg)
  "Open FILE as root.

This will prompt you to save the current buffer, unless prefix ARG is given, in
which case it will save it without prompting."
  (interactive
   (list (read-file-name "Open file as root: ")
         current-prefix-arg))
  ;; HACK: Teach `save-place' to treat the new "remote" buffer as if it were
  ;;   visiting the same local file (because it is), and preserve the cursor
  ;;   position as usual.
  (letf! ((defun remote-local-name (path)
            (if path (or (file-remote-p path 'localname) path)))
          (defmacro with-local-name (&rest body)
            `(when save-place-mode
               (let ((buffer-file-name (remote-local-name buffer-file-name))
                     (default-directory (remote-local-name default-directory)))
                 ,@body))))
    (let ((window-start (window-start))
          (buffer (current-buffer)))
      (when (and buffer-file-name (file-equal-p buffer-file-name file))
        (when (buffer-modified-p)
          (save-some-buffers arg (lambda () (eq (current-buffer) buffer))))
        (with-local-name (save-place-to-alist)))
      (prog1
          ;; HACK: Disable auto-save in temporary tramp buffers because it could
          ;;   trigger processes that hang silently in the background, making
          ;;   those buffers inoperable for the rest of that session (Tramp
          ;;   caches them).
          (let ((auto-save-default nil)
                ;; REVIEW: use only these when we drop 28 support
                (remote-file-name-inhibit-auto-save t)
                (remote-file-name-inhibit-auto-save-visited t)
                ;; Prevent redundant work
                save-place-mode)
            (find-file (doom--sudo-file-path (expand-file-name file))))
        ;; Record of the cursor's old position if it isn't at BOB (indicating
        ;; this buffer was already open), in case the user wishes to go to it.
        (unless (bobp)
          (doom-set-jump-h)
          ;; save-place-find-file-hook requires point be a BOB to do its thang.
          (goto-char (point-min)))
        (with-local-name (save-place-find-file-hook))
        (set-window-start nil window-start)))))

;;;###autoload
(defun doom/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (doom/sudo-find-file
   (or (buffer-file-name (buffer-base-buffer))
       (when (or (derived-mode-p 'dired-mode)
                 (derived-mode-p 'wdired-mode))
         default-directory)
       (user-error "Current buffer isn't visiting a file"))))

;;;###autoload
(defun doom/sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (let ((file (doom--sudo-file-path (buffer-file-name (buffer-base-buffer)))))
    (if-let* ((buffer (find-file-noselect file)))
        (let ((origin (current-buffer)))
          (copy-to-buffer buffer (point-min) (point-max))
          (unwind-protect
              (with-current-buffer buffer
                (save-buffer))
            (unless (eq origin buffer)
              (kill-buffer buffer))
            (with-current-buffer origin
              (revert-buffer t t))))
      (user-error "Unable to open %S" file))))

;;;###autoload
(defun doom/remove-recent-file (file)
  "Remove FILE from your recently-opened-files list."
  (interactive
   (list (completing-read "Remove recent file: "
                          (lambda (string predicate action)
                            (if (eq action 'metadata)
                                '(metadata
                                  (display-sort-function . identity)
                                  (cycle-sort-function . identity)
                                  (category . file))
                              (complete-with-action
                               action recentf-list string predicate)))
                          nil t)))
  (setq recentf-list (delete (recentf-expand-file-name file) recentf-list))
  (recentf-save-list)
  (message "Removed %S from `recentf-list'" (abbreviate-file-name file)))


;;
;;; Backports

;; Introduced in Emacs 29.
;;;###autoload
(static-unless (fboundp 'find-sibling-file)
  (defvar find-sibling-rules nil)

  (defun find-sibling-file (file)
    "Visit a \"sibling\" file of FILE.
When called interactively, FILE is the currently visited file.

The \"sibling\" file is defined by the `find-sibling-rules' variable."
    (interactive (progn
                   (unless buffer-file-name
                     (user-error "Not visiting a file"))
                   (list buffer-file-name)))
    (unless find-sibling-rules
      (user-error "The `find-sibling-rules' variable has not been configured"))
    (let ((siblings (find-sibling-file-search (expand-file-name file)
                                              find-sibling-rules)))
      (cond
       ((null siblings)
        (user-error "Couldn't find any sibling files"))
       ((length= siblings 1)
        (find-file (car siblings)))
       (t
        (let ((relatives (mapcar (lambda (sibling)
                                   (file-relative-name
                                    sibling (file-name-directory file)))
                                 siblings)))
          (find-file
           (completing-read (format-prompt "Find file" (car relatives))
                            relatives nil t nil nil (car relatives))))))))

  (defun find-sibling-file-search (file &optional rules)
    "Return a list of FILE's \"siblings\".
RULES should be a list on the form defined by `find-sibling-rules' (which
see), and if nil, defaults to `find-sibling-rules'."
    (let ((results nil))
      (pcase-dolist (`(,match . ,expansions) (or rules find-sibling-rules))
        ;; Go through the list and find matches.
        (when (string-match match file)
          (let ((match-data (match-data)))
            (dolist (expansion expansions)
              (let ((start 0))
                ;; Expand \\1 forms in the expansions.
                (while (string-match "\\\\\\([&0-9]+\\)" expansion start)
                  (let ((index (string-to-number (match-string 1 expansion))))
                    (setq start (match-end 0)
                          expansion
                          (replace-match
                           (substring file
                                      (elt match-data (* index 2))
                                      (elt match-data (1+ (* index 2))))
                           t t expansion)))))
              ;; Then see which files we have that are matching.  (And
              ;; expand from the end of the file's match, since we might
              ;; be doing a relative match.)
              (let ((default-directory (substring file 0 (car match-data))))
                ;; Keep the first matches first.
                (setq results
                      (nconc
                       results
                       (mapcar #'expand-file-name
                               ;; `file-expand-wildcards' has a new REGEXP
                               ;; argument in 29+ that is needed here. This swap
                               ;; makes it behave as if REGEXP == t.
                               (letf! (defun wildcard-to-regexp (wildcard)
                                        (concat "\\`" wildcard "\\'"))
                                 (file-expand-wildcards expansion nil))))))))))
      ;; Delete the file itself (in case it matched), and remove
      ;; duplicates, in case we have several expansions and some match
      ;; the same subsets of files.
      (delete file (delete-dups results)))))

(provide 'doom-lib '(files))
;;; files.el ends here
