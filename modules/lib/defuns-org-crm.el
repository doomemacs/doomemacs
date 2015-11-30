;;; defuns-org-crm.el --- for my custom org-based CRM

(defun narf--helm-org (&optional directory)
  (let ((helm-deft-dir-list `(,(or directory default-directory))))
    (helm-deft)))

;;;###autoload
(defun narf/helm-org ()
  (interactive)
  (let ((default-directory org-directory))
    (helm-projectile-find-file)))

;;;###autoload
(defun narf/helm-org-crm-projects ()
  (interactive)
  (narf--helm-org org-directory-projects))

;;;###autoload
(defun narf/helm-org-crm-contacts ()
  (interactive)
  (narf--helm-org org-directory-contacts))

;;;###autoload
(defun narf/helm-org-crm-invoices ()
  (interactive)
  (narf--helm-org org-directory-invoices))

;;;###autoload
(defun narf/helm-org-writing ()
  (interactive)
  (let ((narf--helm-org-params '()))
    (narf--helm-org (expand-file-name "writing/" org-directory))))

;;;###autoload
(defun narf/org-crm-link-contact (id)
  (org-open-file (narf--org-crm-resolve-id id 'contact) t))
;;;###autoload
(defun narf/org-crm-link-project (id)
  (org-open-file (narf--org-crm-resolve-id id 'project) t))
;;;###autoload
(defun narf/org-crm-link-invoice (id)
  (org-open-file (narf--org-crm-resolve-id id 'invoice) t))

(defun narf--org-complete (type)
  (let ((default-directory (symbol-value (intern (format "org-directory-%ss" type)))))
    (let* ((file (org-iread-file-name ">>> "))
           (match (s-match "^\\([0-9]+\\)[-.]" (f-filename file))))
      (unless (file-exists-p file)
        (message "Created %s" file)
        (write-region "" nil file))
      (unless match
        (user-error "Invalid file ID"))
      (format "%s:%s" type (cadr match)))))

;;;###autoload
(defun org-contact-complete-link () (narf--org-complete "contact"))
;;;###autoload
(defun org-project-complete-link () (narf--org-complete "project"))
;;;###autoload
(defun org-invoice-complete-link () (narf--org-complete "invoice"))

(defun narf--org-crm-new-path (name type)
  (unless (memq type '(project contact invoice))
    (user-error "Not a valid type: %s" type))
  (let* ((invoice-p (eq type 'invoice))
         (prefix
          (replace-regexp-in-string
           "/+$" "" (symbol-value (intern (format "org-directory-%ss" (symbol-name type))))))
         (last-file (car-safe (f-glob "*.org" prefix)))
         (id-sep (string-match-p (if invoice-p "\\." "-") last-file)))
    (if last-file
        (let* ((old-id (string-to-number (f-filename (substring last-file 0 id-sep)) 16))
               (new-id (format (if invoice-p "%X" "%03X") (1+ old-id))))
          (format (if invoice-p "%s/%s.org" "%s/%s-%s.org") prefix new-id
                  (replace-regexp-in-string "[][ !@#$%^&*()]" "-" name)))
      (user-error "path could not be resolved: type(%s) name(%s)" type name))))

(defun narf--org-crm-resolve-id (id type)
  (unless (memq type '(project contact invoice))
    (user-error "Not a valid type: %s" type))
  (let* ((invoice-p (eq type 'invoice))
         (prefix
          (replace-regexp-in-string
           "/+$" "" (symbol-value (intern (format "org-directory-%ss" (symbol-name type)))))))
    (or (car-safe
         (f-glob (format (if invoice-p "%X.org" "%03X*.org") (string-to-number id 16))
                 org-directory-projects))
        (user-error "id %s could not be resolved in %s" id type))))

(defun narf--org-crm (&optional id type new-p)
  (if (not id)
      (funcall (intern (format "narf/helm-org-crm-%ss" type)))
    (let ((file (narf--org-crm-resolve-id id type)))
      (when (evil-visual-state-p)
        (org-insert-link
         nil (format "%s:%s" (symbol-name type) id)
         (buffer-substring-no-properties (region-beginning) (region-end))))
      (if new-p
          (when (y-or-n-p (format "Create %s?" (f-filename file)))
            (find-file file))
        (find-file file)))))


;;;###autoload (autoload 'narf:org-crm-project "defuns-org-crm" nil t)
(evil-define-command narf:org-crm-project (&optional bang name)
  (interactive "<!><a>")
  (narf--org-crm name 'project bang))
;;;###autoload (autoload 'narf:org-crm-contact "defuns-org-crm" nil t)
(evil-define-command narf:org-crm-contact (&optional bang name)
  (interactive "<!><a>")
  (narf--org-crm name 'contact bang))
;;;###autoload (autoload 'narf:org-crm-invoice "defuns-org-crm" nil t)
(evil-define-command narf:org-crm-invoice (&optional bang)
  (interactive "<!>")
  (narf--org-crm "" 'invoice bang))


(provide 'defuns-org-crm)
;;; defuns-org-crm.el ends here
