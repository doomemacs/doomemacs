;;; lang/python/autoload.el

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  (process-buffer (run-python python-shell-interpreter t t)))
