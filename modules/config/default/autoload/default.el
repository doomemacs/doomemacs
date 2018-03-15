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
  (interactive) (doom-project-browse +org-dir))
;;;###autoload
(defun +default/find-in-notes ()
  (interactive) (doom-project-find-file +org-dir))

;;;###autoload
(defun +default/browse-snippets ()
  (interactive) (doom-project-browse emacs-snippets-dir))
;; NOTE No need for a browse-snippets variant, use `yas-visit-snippet-file'


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
