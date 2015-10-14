;;; defuns-company.el

;;;###autoload
(defun narf/company-evil-complete-next (&optional arg)
  (call-interactively 'company-dabbrev)
  (if (eq company-candidates-length 1)
      (company-complete)))

;;;###autoload
(defun narf/company-evil-complete-previous (&optional arg)
  (let ((company-selection-wrap-around t))
    (call-interactively 'company-dabbrev)
    (if (eq company-candidates-length 1)
        (company-complete)
      (call-interactively 'company-select-previous))))

(provide 'defuns-company)
;;; defuns-company.el ends here
