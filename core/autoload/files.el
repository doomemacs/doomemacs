;;; core/autoload/files.el -*- lexical-binding: t; -*-

(defun doom--path (&rest segments)
  (let (file-name-handler-alist)
    (let ((dir (pop segments)))
      (unless segments
        (setq dir (expand-file-name dir)))
      (while segments
        (setq dir (expand-file-name (car segments) dir)
              segments (cdr segments)))
      dir)))

;;;###autoload
(defun doom-glob (&rest segments)
  "Construct a path from SEGMENTS and expand glob patterns.
Returns nil if the path doesn't exist."
  (let* (case-fold-search
         file-name-handler-alist
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
  (when-let* ((path (apply #'doom-path segments)))
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
  (let (file-name-handler-alist
        result)
    (dolist (file (mapcan (doom-rpartial #'doom-glob "*") (doom-enlist paths)))
      (cond ((file-directory-p file)
             (nconcq! result
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


;;
;;; Helpers

(defun doom--forget-file (old-path &optional new-path)
  "Ensure `recentf', `projectile' and `save-place' forget OLD-PATH."
  (when (bound-and-true-p recentf-mode)
    (when new-path
      (recentf-add-file new-path))
    (recentf-remove-if-non-kept old-path))
  (when (and (bound-and-true-p projectile-mode)
             (doom-project-p)
             (projectile-file-cached-p old-path (doom-project-root)))
    (projectile-purge-file-from-cache old-path))
  (when (bound-and-true-p save-place-mode)
    (save-place-forget-unreadable-files)))

(defun doom--update-file (path)
  (when (featurep 'vc)
    (vc-file-clearprops path)
    (vc-resynch-buffer path nil t))
  (when (featurep 'magit)
    (magit-refresh)))

(defun doom--copy-file (old-path new-path &optional force-p)
  (let* ((new-path (expand-file-name new-path))
         (old-path (file-truename old-path))
         (new-path (apply #'expand-file-name
                          (if (or (directory-name-p new-path)
                                  (file-directory-p new-path))
                              (list (file-name-nondirectory old-path) new-path)
                            (list new-path))))
         (new-path-dir (file-name-directory new-path))
         (project-root (doom-project-root))
         (short-new-name (if (and project-root (file-in-directory-p new-path project-root))
                             (file-relative-name new-path project-root)
                           (abbreviate-file-name new-path))))
    (unless (file-directory-p new-path-dir)
      (make-directory new-path-dir t))
    (when (buffer-modified-p)
      (save-buffer))
    (cond ((file-equal-p old-path new-path)
           (throw 'status 'overwrite-self))
          ((and (file-exists-p new-path)
                (not force-p)
                (not (y-or-n-p (format "File already exists at %s, overwrite?" short-new-name))))
           (throw 'status 'aborted))
          ((file-exists-p old-path)
           (copy-file old-path new-path t)
           short-new-name)
          (short-new-name))))


;;
;;; Commands

;;;###autoload
(defun doom/delete-this-file (&optional path force-p)
  "Delete FILENAME (defaults to the file associated with current buffer) and
kills the buffer. If FORCE-P, force the deletion (don't ask for confirmation)."
  (interactive
   (list (file-truename (buffer-file-name))
         current-prefix-arg))
  (let* ((fbase (file-name-sans-extension (file-name-nondirectory path)))
         (buf (current-buffer)))
    (cond ((not (file-exists-p path))
           (error "File doesn't exist: %s" path))
          ((not (or force-p (y-or-n-p (format "Really delete %s?" fbase))))
           (message "Aborted")
           nil)
          ((unwind-protect
               (progn (delete-file path) t)
             (let ((short-path (file-relative-name path (doom-project-root))))
               (if (file-exists-p path)
                   (error "Failed to delete %s" short-path)
                 ;; Ensures that windows displaying this buffer will be switched
                 ;; to real buffers (`doom-real-buffer-p')
                 (doom/kill-this-buffer-in-all-windows buf t)
                 (doom--forget-file path)
                 (doom--update-file path)
                 (message "Successfully deleted %s" short-path))))))))

;;;###autoload
(defun doom/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH. If FORCE-P, overwrite the destination
file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (pcase (catch 'status
           (when-let* ((dest (doom--copy-file (buffer-file-name) new-path force-p)))
             (doom--update-file new-path)
             (message "File successfully copied to %s" dest)))
    (`overwrite-self (error "Cannot overwrite self"))
    (`aborted (message "Aborted"))
    (_ t)))

;;;###autoload
(defun doom/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH. If FORCE-P, overwrite the destination
file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (pcase (catch 'status
           (let ((old-path (buffer-file-name))
                 (new-path (expand-file-name new-path)))
             (when-let* ((dest (doom--copy-file old-path new-path force-p)))
               (when (file-exists-p old-path)
                 (delete-file old-path))
               (kill-current-buffer)
               (doom--forget-file old-path new-path)
               (doom--update-file new-path)
               (find-file new-path)
               (message "File successfully moved to %s" dest))))
    (`overwrite-self (error "Cannot overwrite self"))
    (`aborted (message "Aborted"))
    (_ t)))

;;;###autoload
(defun doom/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":" (file-remote-p file 'user) "@" (file-remote-p file 'host)  "|sudo:root@" (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

;;;###autoload
(defun doom/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (doom/sudo-find-file (file-truename buffer-file-name)))
