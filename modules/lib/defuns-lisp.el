;;; defuns-lisp.el

;;;###autoload
(defun narf/elisp-find-function-at-pt ()
  (interactive)
  (let ((func (function-called-at-point)))
    (if func (find-function func))))

;;;###autoload
(defun narf/elisp-find-function-at-pt-other-window ()
  (interactive)
  (let ((func (function-called-at-point)))
    (if func (find-function-other-window func))))

(provide 'defuns-lisp)
;;; defuns-lisp.el ends here
