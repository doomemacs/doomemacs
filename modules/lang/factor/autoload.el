;;; lang/factor/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +factor/open-repl ()
  "Open the Factor Listener."
  (interactive)
  (call-interactively #'run-factor)
  (current-buffer))
