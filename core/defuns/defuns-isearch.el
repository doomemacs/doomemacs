
;;;###autoload
(defun narf/isearch-delete-word ()
  (interactive)
  (let ((num (length isearch-string))
        (string (s-reverse isearch-string)))
    (when (string-match "[^a-zA-Z0-9]" string 1)
      (setq num (match-beginning 0)))
    (dotimes (i num)
      (isearch-pop-state))
    (isearch-update)))

;;;###autoload
(defun narf/isearch-delete-line ()
  (interactive)
  (let ((num (length isearch-string)))
    (dotimes (i num) (isearch-pop-state))
    (isearch-update)))

;;;###autoload
(defun narf/isearch-paste-from-register (reg)
  (interactive)
  (let ((str (evil-get-register reg t)))
    (when (> (length str) 0)
      (isearch-yank-string str))))

;;;###autoload
(defun narf/isearch-paste-from-clipboard ()
  (interactive)
  (narf:isearch-paste-from-register ?+))


(provide 'defuns-isearch)
;;; defuns-isearch.el ends here
