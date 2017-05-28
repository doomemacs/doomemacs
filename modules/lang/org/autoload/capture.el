;;; lang/org/autoload/capture.el

;;;###autoload
(defun +org/capture (&optional key string)
  "Initializes the current frame as a pop-up `org-capture' frame."
  (interactive)
  (let ((key (or key "n"))
        (string (unless (string-empty-p string) string)))
    (if string
        (org-capture-string string key)
      (org-capture nil key))))
