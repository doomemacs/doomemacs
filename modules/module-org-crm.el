;;; module-org-crm.el

(add-hook 'org-load-hook 'doom|org-crm-init)

(defvar org-directory-crm (expand-file-name "/crm/" org-directory))

(defun doom|org-crm-init ()
  ;; (define-org-section! work "Work")
  ;; (define-org-section! project "Projects")
  ;; (define-org-section! contact "Contacts")
  )

;;
(defun doom/org-crm-new (type name)
  )

(defun doom/org-crm-visit-project (&optional name)
  )

(defun doom/org-crm-visit-contact (&optional name)
  )

(defun doom/org-crm-visit-invoice (&optional name)
  )

(provide 'module-org-crm)
;;; module-org-crm.el ends here
