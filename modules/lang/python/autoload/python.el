;;; lang/python/autoload/python.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  (process-buffer (run-python nil t t)))
