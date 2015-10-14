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

;;;###autoload
(defun narf/company-complete-common-or-complete-full ()
  (interactive)
  (when (company-manual-begin)
    (if (eq last-command #'company-complete-common-or-cycle)
        (let ((company-selection-wrap-around t))
          (call-interactively #'company-complete-selection))
      (let ((buffer-mod-tick (buffer-chars-modified-tick)))
        (call-interactively #'company-complete-common)
        (when (= buffer-mod-tick (buffer-chars-modified-tick))
          (call-interactively #'company-complete-selection)
          (call-interactively #'company-complete))))))

(provide 'defuns-company)
;;; defuns-company.el ends here
