;;; core/autoload/scratch.el -*- lexical-binding: t; -*-

(defvar doom-scratch-files-dir (concat doom-etc-dir "scratch/")
  "Where to store project scratch files, created by
`doom/open-project-scratch-buffer'.")

(defvar doom-scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is made.")

(defun doom--create-scratch-buffer ()
  (let* ((project-p (doom-project-p 'nocache))
         (text (and (region-active-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))))
         (mode major-mode)
         (derived-p (derived-mode-p 'prog-mode 'text-mode))
         (project-root (if project-p (doom-project-root 'nocache)))
         (id (projectile-project-name)))
    (when (and id (string-empty-p id))
      (user-error "Invalid id for a scratch buffer (%s)" id))
    (unless (file-directory-p doom-scratch-files-dir)
      (make-directory doom-scratch-files-dir t))
    (with-current-buffer
        (if project-p
            (find-file-noselect
             (expand-file-name (replace-regexp-in-string
                                "\\." "_" id
                                t t)
                               doom-scratch-files-dir)
             nil t)
          (get-buffer-create "*doom:scratch*"))
      (when project-p
        (rename-buffer (format "*doom:scratch (%s)*" id))
        (setq default-directory project-root))
      (when (and (not (eq major-mode mode))
                 derived-p
                 (functionp mode))
        (funcall mode))
      (if text (insert text))
      (run-hooks 'doom-scratch-buffer-hook)
      (current-buffer))))

;;;###autoload
(defun doom/open-scratch-buffer (&optional arg)
  "Opens a persistent scratch buffer in the same major-mode, at the current
project root. If no project is found, scratch buffer will not be persistent. If
a selection is active, copy it to the scratch buffer.

If ARG (universal argument) is non-nil, display the buffer in the current
window. Otherwise, display it in a pop up window.

Persistent scratch buffers are stored in `doom-scratch-files-dir'."
  (interactive "P")
  (funcall (if arg #'switch-to-buffer #'display-buffer)
           (doom--create-scratch-buffer)))

