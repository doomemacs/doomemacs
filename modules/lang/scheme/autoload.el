;;; lang/scheme/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +scheme/open-repl (&optional arg)
  "Open the Scheme REPL."
  (interactive "P")
  (switch-to-geiser arg)
  (current-buffer))
