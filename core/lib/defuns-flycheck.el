;;; defuns-flycheck.el
;; for ../core-flycheck.el

;;;###autoload
(defun narf*flycheck-buffer ()
  (when (and (featurep 'flycheck) flycheck-mode)
    (flycheck-buffer)))

;;;###autoload
(defun narf/flycheck-next-error ()
  (interactive)
  (call-interactively
   (if (bound-and-true-p flycheck-mode)
       'flycheck-next-error
     'next-error)))

;;;###autoload
(defun narf/flycheck-previous-error ()
  (interactive)
  (call-interactively
   (if (bound-and-true-p flycheck-mode)
       'flycheck-previous-error
     'previous-error)))

(provide 'defuns-flycheck)
;;; defuns-flycheck.el ends here
