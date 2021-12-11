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
  "Invoke `man' if man is installed and the platform is not MacOS, otherwise use `woman'.

`man -k \"^\"` is very slow on MacOS, which is what `Man-completion-table' uses to
generate `completing-read' candidates."
  (interactive)
  (call-interactively
   (if (and (not IS-MAC) (executable-find "man"))
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
(defun +default/restart-server ()
  "Restart the Emacs server."
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))

;;;###autoload
(defun +default/diagnostics (&rest arg)
  "List diagnostics for the current buffer/project.
If the the vertico and lsp modules are active, list lsp diagnostics for the
current project. Otherwise list them for the current buffer"
  (interactive)
  (cond ((and (featurep! :completion vertico)
              (featurep! :tools lsp)
              (bound-and-true-p lsp-mode))
         (consult-lsp-diagnostics arg))
        ((and (featurep! :checkers syntax)
              (bound-and-true-p flycheck-mode))
         (flycheck-list-errors))
        ((bound-and-true-p flymake-mode)
         (flymake-show-diagnostics-buffer))
        (t
         (user-error "No diagnostics backend detected. Enable flycheck or \
flymake, or set up lsp-mode if applicable (see :lang lsp)"))))
