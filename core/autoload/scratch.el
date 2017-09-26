;;; core/autoload/scratch.el -*- lexical-binding: t; -*-

(defvar doom-scratch-files-dir (concat doom-etc-dir "scratch/")
  "Where to store project scratch files, created by
`doom/open-project-scratch-buffer'.")

(defvar doom-scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is made.")

(defun doom--create-scratch-buffer (&optional project-p)
  (let ((text (and (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))))
        (mode major-mode)
        (derived-p (derived-mode-p 'prog-mode 'text-mode))
        (old-project (doom-project-root)))
    (unless (file-directory-p doom-scratch-files-dir)
      (mkdir doom-scratch-files-dir t))
    (with-current-buffer
        (if project-p
            (find-file-noselect
             (expand-file-name (replace-regexp-in-string
                                "\\." "_" (projectile-project-name)
                                t t)
                               doom-scratch-files-dir)
             nil t)
          (get-buffer-create "*doom:scratch*"))
      (when project-p
        (rename-buffer (format "*doom:scratch (%s)*" (projectile-project-name))))
      (setq default-directory old-project)
      (when (and (not (eq major-mode mode))
                 derived-p
                 (functionp mode))
        (funcall mode))
      (if text (insert text))
      (run-hooks 'doom-scratch-buffer-hook)
      (current-buffer))))

;;;###autoload
(defun doom/open-scratch-buffer ()
  "Opens a temporary scratch buffer in a popup window. It is discarded once it
is closed. If a region is active, copy it to the scratch buffer."
  (interactive)
  (doom-popup-buffer (doom--create-scratch-buffer)))

;;;###autoload
(defun doom/open-project-scratch-buffer ()
  "Opens a (persistent) scratch buffer associated with the current project in a
popup window. Scratch buffers are stored in `doom-scratch-files-dir'. If a
region is active, copy it to the scratch buffer."
  (interactive)
  (doom-popup-buffer (doom--create-scratch-buffer t)))

