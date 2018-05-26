;;; lang/python/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  (process-buffer (run-python nil t t)))

;;;###autoload
(defun +python/repl-send-dwim ()
  "send aynthing I want"
  (interactive)
  (cond ((string-equal
          (symbol-at-point)
          "def")
         (python-shell-send-defun)
         (python-nav-beginning-of-defun
          -1))
        (t
         (python-shell-send-region
          (python-nav-beginning-of-statement)
          (python-nav-end-of-statement))
         (python-nav-forward-statement))))
