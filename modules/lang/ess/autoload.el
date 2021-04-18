;;; lang/ess/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ess/open-julia-repl (&optional arg)
  "Open an ESS Julia REPL"
  (interactive "P")
  (run-ess-julia arg)
  (current-buffer))

;;;###autoload
(defun +ess/open-r-repl (&optional arg)
  "Open an ESS R REPL"
  (interactive "P")
  (run-ess-r arg)
  (current-buffer))
