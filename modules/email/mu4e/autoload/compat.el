;;; email/mu4e/autoload/compat.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mu4e-compose-goto-top ()
  "Go to the beginning of the message or buffer.
Go to the beginning of the message or, if already there, go to the
beginning of the buffer."
  (interactive)
  (let ((old-position (point)))
    (message-goto-body)
    (when (equal (point) old-position)
      (beginning-of-buffer))))

;;;###autoload
(defun mu4e-compose-goto-bottom ()
  "Go to the end of the message or buffer.
Go to the end of the message (before signature) or, if already there, go to the
end of the buffer."
  (interactive)
  (let ((old-position (point))
        (message-position (save-excursion (message-goto-body) (point))))
    (end-of-buffer)
    (when (re-search-backward "^-- $" message-position t)
      (previous-line))
    (when (equal (point) old-position)
      (end-of-buffer))))
