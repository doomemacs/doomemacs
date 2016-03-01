;;; module-org-crm.el

(defun narf|org-crm-init ()
  (define-org-section! cproject "Work")
  (define-org-section! project "Projects")
  (define-org-section! contact "Contacts")
  )

(defun narf/org-crm-new (type name)
  )

(defun narf/org-crm-visit-project (&optional name)
  )

(defun narf/org-crm-visit-contact (&optional name)
  )

(defun narf/org-crm-visit-invoice (&optional name)
  )

(provide 'module-org-crm)
;;; module-org-crm.el ends here
