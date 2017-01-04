;;; defuns-magit.el

;;;###autoload
(defun doom/magit-pop-to-buffer (buffer)
  "Pop to buffer in non-magit buffer."
  (let (pt)
    (doom/popup-save
     (pop-to-buffer buffer)
     (setq pt (point)))
    (goto-char pt)))

(provide 'defuns-magit)
;;; defuns-magit.el ends here
