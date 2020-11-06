;;; core/autoload/files.el -*- lexical-binding: t; -*-

(defun doom--resolve-path-forms (spec &optional directory)
  "Converts a simple nested series of or/and forms into a series of
`file-exists-p' checks.

For example

  (doom--resolve-path-forms
    '(or A (and B C))
    \"~\")

Returns (approximately):

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
            (mapcar (doom-rpartial #'doom--resolve-path-forms directory)
                    (cdr spec)))
    (let ((filevar (make-symbol "file")))
      `(let ((,filevar ,spec))
         (and (stringp ,filevar)
              ,(if directory
                   `(let ((default-directory ,directory))
                      (file-exists-p ,filevar))
                 `(file-exists-p ,filevar))
              ,filevar)))))

(defun doom--path (&rest segments)
  (let ((dir (pop segments)))
    (unless segments
      (setq dir (expand-file-name dir)))
    (while segments
      (setq dir (expand-file-name (car segments) dir)
            segments (cdr segments)))
    dir))

;;;###autoload
(defun doom-glob (&rest segments)
  "Construct a path from SEGMENTS and expand glob patterns.
Returns nil if the path doesn't exist."
  (let* (case-fold-search
         (dir (apply #'doom--path segments)))
    (if (string-match-p "[[*?]" dir)
        (file-expand-wildcards dir t)
      (if (file-exists-p dir)
          dir))))

;;;###autoload
(defun doom-path (&rest segments)
  "Constructs a file path from SEGMENTS."
  (if segments
      (apply #'doom--path segments)
    (file!)))

;;;###autoload
(defun doom-dir (&rest segments)
  "Constructs a path from SEGMENTS.
See `doom-path'."
  (when-let (path (apply #'doom-path segments))
    (directory-file-name (file-name-directory path))))

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
    (dolist (file (mapcan (doom-rpartial #'doom-glob "*") (doom-enlist paths)))
      (cond ((file-directory-p file)
             (appendq!
              result
              (and (memq type '(t dirs))
                   (string-match-p match file)
                   (not (and filter (funcall filter file)))
                   (not (and (file-symlink-p file)
                             (not follow-symlinks)))
                   (<= mindepth 0)
                   (list (cond (map (funcall map file))
                               (relative-to (file-relative-name file relative-to))
                               (file))))
              (and (>= depth 1)
                   (apply #'doom-files-in file
                          (append (list :mindepth (1- mindepth)
                                        :depth (1- depth)
                                        :relative-to relative-to)
                                  rest)))))
            ((and (memq type '(t files))
                  (string-match-p match file)
                  (not (and filter (funcall filter file)))
                  (<= mindepth 0))
             (push (if relative-to
                       (file-relative-name file relative-to)
                     file)
                   result))))
    result))

;;;###autoload
(defun doom-file-cookie-p (file &optional cookie null-value)
  "Returns the evaluated result of FORM in a ;;;###COOKIE FORM at the top of
FILE.

If COOKIE doesn't exist, return NULL-VALUE."
  (unless (file-exists-p file)
    (signal 'file-missing file))
  (unless (file-readable-p file)
    (error "%S is unreadable" file))
  (with-temp-buffer
    (insert-file-contents file nil 0 256)
    (if (re-search-forward (format "^;;;###%s " (regexp-quote (or cookie "if")))
                           nil t)
        (let ((load-file-name file))
          (eval (sexp-at-point) t))
      null-value)))

;;;###autoload
(defmacro file-exists-p! (files &optional directory)
  "Returns non-nil if the FILES in DIRECTORY all exist.

DIRECTORY is a path; defaults to `default-directory'.

Returns the last file found to meet the rules set by FILES, which can be a
single file or nested compound statement of `and' and `or' statements."
  `(let ((p ,(doom--resolve-path-forms files directory)))
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
;;; Helpers

(defun doom--update-files (&rest files)
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
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
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
          (doom--update-files path)
          (message "Deleted %S" short-path))))))

;;;###autoload
(defun doom/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH.

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
    (doom--update-files old-path new-path)
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
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (doom--update-files old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

(defun doom--sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

;;;###autoload
(defun doom/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (doom--sudo-file-path file)))

;;;###autoload
(defun doom/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-file
   (doom--sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

;;;###autoload
(defun doom/sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (let ((file (doom--sudo-file-path buffer-file-name)))
    (if-let (buffer (find-file-noselect file))
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
