;;; tools/llm/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +llm/open-in-same-window ()
  "Open gptel buffer in the selected buffer."
  (interactive)
  (let ((gptel-display-buffer-action '(display-buffer-same-window)))
    (call-interactively #'gptel)))
