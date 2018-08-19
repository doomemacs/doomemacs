;;; lang/ess/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ess/r-repl ()
  "Open the R REPL."
  (interactive)
  (inferior-ess nil nil t))
