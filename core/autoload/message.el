;;; message.el

;;;###autoload
(defun doom-message-buffer ()
  (let ((buffer (get-buffer-create " *doom-messages*")))
    (unless (get-buffer-window-list buffer)
      (doom-popup-buffer buffer))))

;;;###autoload
(defun doom-message (format &rest args)
  (if noninteractive
      (apply 'message format args)
    (with-current-buffer (doom-message-buffer)
      (insert (format format args)))))

