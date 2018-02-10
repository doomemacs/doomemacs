;;; private/r/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +r/repl ()
  "Open the R REPL."
  (interactive)
  (inferior-ess nil nil t))
