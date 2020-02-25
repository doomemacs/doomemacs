;; config/default/autoload/default.el -*- lexical-binding: t; -*-

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
(defun +default/new-buffer ()
  "TODO"
  (interactive)
  (if (featurep! 'evil)
      (call-interactively #'evil-buffer-new)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))

;;;###autoload
(defun +default/lsp-format-region-or-buffer ()
  "Format the buffer (or selection) with LSP."
  (interactive)
  (unless (bound-and-true-p lsp-mode)
    (user-error "Not in an LSP buffer"))
  (call-interactively
   (if (doom-region-active-p)
       #'lsp-format-region
     #'lsp-format-buffer)))

;;;###autoload
(defun +default/restart-server ()
  "Restart the Emacs server."
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))
