;;; core/autoload/scratch.el -*- lexical-binding: t; -*-

(defvar doom-scratch-files-dir (concat doom-etc-dir "scratch/")
  "Where to store project scratch files, created by
`doom/open-project-scratch-buffer'.")

(defvar doom-scratch-buffer-display-fn #'display-buffer
  "The function to use to display the scratch buffer. Must accept one argument:
the buffer to display.")

(defvar doom-scratch-buffer-major-mode nil
  "What major mode to use in scratch buffers. This can be one of the
following:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar doom-scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is made.")


;;
;; Library

;;;###autoload
(defun doom-scratch-buffer (&optional file mode text)
  "Return a scratchpad buffer in major MODE with TEXT in it.

If FILE is a valid path, open it as if it were a persistent scratchpad."
  (if file (setq file (file-truename file)))
  (let ((buffer
         (if file
             (with-current-buffer (find-file-noselect file)
               (rename-buffer (format "*doom:scratch (%s)*" (file-name-nondirectory file)))
               (current-buffer))
           (get-buffer-create "*doom:scratch*"))))
    (with-current-buffer buffer
      (when (and (functionp mode)
                 (not (eq major-mode mode)))
        (funcall mode))
      (when text
        (insert text))
      (run-hooks 'doom-scratch-buffer-hook)
      (current-buffer))))

;;;###autoload
(defun doom/open-scratch-buffer (&optional arg)
  "Opens a scratch pad window in the same major-mode.

If ARG (universal argument), then open a persistent scratch pad buffer. You'll
be prompted for its name, or to open a previously created. These are stored in
`doom-scratch-files-dir'.

If a region is active, copy its contents to the scratch pad."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     doom-scratch-buffer-display-fn
     (doom-scratch-buffer
      (when arg
        (if-let* ((file (read-file-name "Open scratch file > " doom-scratch-files-dir "scratch")))
            file
          (user-error "Aborting")))
      (cond ((eq doom-scratch-buffer-major-mode t)
             (unless (or buffer-read-only
                         (derived-mode-p 'special-mode)
                         (string-match-p "^ ?\\*" (buffer-name)))
               major-mode))
            ((null doom-scratch-buffer-major-mode) nil)
            ((symbolp doom-scratch-buffer-major-mode)
             doom-scratch-buffer-major-mode))
      (and (region-active-p)
           (buffer-substring-no-properties
            (region-beginning) (region-end)))))))

;;;###autoload
(defun doom/switch-to-scratch-buffer (&optional arg)
  "Switches to a scratch pad buffer in the current window.

Otherwise, does exactly what `doom/open-scratch-buffer' does."
  (interactive "P")
  (let ((doom-scratch-buffer-display-fn #'switch-to-buffer))
    (doom/open-scratch-buffer arg)))

;;;###autoload
(defun doom/delete-scratch-files ()
  "Deletes all scratch buffers in `doom-scratch-files-dir'."
  (interactive)
  (dolist (file (directory-files doom-scratch-files-dir t "^[^.]" t))
    (delete-file file)
    (message "Deleted '%s'" (file-name-nondirectory file))))
