;;; lang/ess/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ess-repl-buffer (&optional start-args)
  "Returns an R/Julia REPL buffer."
  (interactive "P")
  (pcase major-mode
    ('ess-r-mode (run-ess-r start-args))
    ('ess-julia-mode (run-ess-julia start-args))
    (_ (inferior-ess nil nil t)))
  (current-buffer))
