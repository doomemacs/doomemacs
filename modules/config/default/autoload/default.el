;; config/default/autoload/default.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +default/browse-project ()
  (interactive) (doom-project-browse (doom-project-root)))
;; NOTE No need for find-in-project, use `projectile-find-file'

;;;###autoload
(defun +default/browse-templates ()
  (interactive) (doom-project-browse +file-templates-dir))
;;;###autoload
(defun +default/find-in-templates ()
  (interactive) (doom-project-find-file +file-templates-dir))

;;;###autoload
(defun +default/browse-emacsd ()
  (interactive) (doom-project-browse doom-emacs-dir))
;;;###autoload
(defun +default/find-in-emacsd ()
  (interactive) (doom-project-find-file doom-emacs-dir))

;;;###autoload
(defun +default/browse-notes ()
  (interactive) (doom-project-browse org-directory))
;;;###autoload
(defun +default/find-in-notes ()
  (interactive) (doom-project-find-file org-directory))

;;;###autoload
(defun +default/find-in-config ()
  "Open a file somewhere in `doom-private-dir' via a fuzzy filename search."
  (interactive)
  (doom-project-find-file doom-private-dir))

;;;###autoload
(defun +default/browse-config ()
  "Browse the files in `doom-private-dir'."
  (interactive)
  (doom-project-browse doom-private-dir))

;;;###autoload
(defun +default/compile (arg)
  "Runs `compile' from the root of the current project.

If a compilation window is already open, recompile that instead.

If ARG (universal argument), runs `compile' from the current directory."
  (interactive "P")
  (if (and (bound-and-true-p compilation-in-progress)
           (buffer-live-p compilation-last-buffer))
      (recompile)
    (call-interactively
     (if arg
         #'projectile-compile-project
       #'compile))))

;;;###autoload
(defun +default/man-or-woman ()
  "Invoke `man' if man is installed, otherwise use `woman'."
  (interactive)
  (call-interactively
   (if (executable-find "man")
       #'man
     #'woman)))

;;;###autoload
(defalias '+default/newline #'newline)

;;;###autoload
(defun +default/new-buffer ()
  "TODO"
  (interactive)
  (if (featurep! 'evil)
      (call-interactively #'evil-buffer-new)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))
