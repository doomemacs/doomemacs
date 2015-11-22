;;; defuns-org-custom.el -- custom functions, links, etc. for Org-mode

;;; Custom links
(defun narf--org-id-to-file (id dir &optional pattern)
  (let* ((glob (f-glob (format (concat "%s" (or pattern "%s-*.org")) dir id)))
         (glob-len (length glob)))
    (when (zerop glob-len)
      (user-error "Could not find file with that ID"))
    (car glob)))

;;;###autoload
(defun narf/org-link-contact (id)
  (org-open-file (narf--org-id-to-file id org-directory-contacts) t))
;;;###autoload
(defun narf/org-link-project (id)
  (org-open-file (narf--org-id-to-file id org-directory-projects) t))
;;;###autoload
(defun narf/org-link-invoice (id)
  (org-open-file (narf--org-id-to-file id org-directory-invoices "%s.org") t))

;;;###autoload
(defun narf/org-complete (type)
  (let ((default-directory (symbol-value (intern (format "org-directory-%ss" type)))))
    (let* ((file (org-iread-file-name ">>> "))
           (match (s-match "^\\([0-9]+\\)[-.]" (f-filename file))))
      (unless match
        (user-error "Invalid file ID"))
      (format "%s:%s" type (cadr match)))))

;;;###autoload
(defun org-contact-complete-link ()
  (narf/org-complete "contact"))
;;;###autoload
(defun org-project-complete-link ()
  (narf/org-complete "project"))
;;;###autoload
(defun org-invoice-complete-link ()
  (narf/org-complete "invoice"))

;;; Deft
;;;###autoload
(defun narf/helm-org-projects ()
  (interactive)
  (narf/helm-org-search org-directory-projects))
;;;###autoload
(defun narf/helm-org-contacts ()
  (interactive)
  (narf/helm-org-search org-directory-contacts))
;;;###autoload
(defun narf/helm-org-invoices ()
  (interactive)
  (narf/helm-org-search org-directory-invoices))
;;;###autoload
(defun narf/helm-org-writing ()
  (interactive)
  (narf/helm-org-search (expand-file-name "writing/" org-directory)))

(provide 'defuns-org-custom)
;;; defuns-org-custom.el ends here
