;;; defuns-flycheck.el
;; for ../core-flycheck.el

;;;###autoload
(defun doom*flycheck-buffer ()
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)))

;;;###autoload
(defun doom/flycheck-next-error ()
  (interactive)
  (call-interactively
   (if (bound-and-true-p flycheck-mode)
       'flycheck-next-error
     'next-error)))

;;;###autoload
(defun doom/flycheck-previous-error ()
  (interactive)
  (call-interactively
   (if (bound-and-true-p flycheck-mode)
       'flycheck-previous-error
     'previous-error)))

;;;###autoload
(defun doom/flycheck-errors ()
  (interactive)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)
    (flycheck-list-errors)))

(provide 'defuns-flycheck)
;;; defuns-flycheck.el ends here
