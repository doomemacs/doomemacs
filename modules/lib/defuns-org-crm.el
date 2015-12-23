;;; defuns-org-crm.el --- for my custom org-based CRM

(defun narf--helm-org (&optional directory)
  (let ((helm-deft-dir-list `(,(or directory default-directory))))
    (helm-deft)))

;;;###autoload
(defun narf/helm-org ()
  (interactive)
  (narf--helm-org org-directory))

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
  (org-open-file (narf--org-crm-id-to-path id 'contact) t))
;;;###autoload
(defun narf/org-crm-link-project (id)
  (org-open-file (narf--org-crm-id-to-path id 'project) t))
;;;###autoload
(defun narf/org-crm-link-invoice (id)
  (org-open-file (narf--org-crm-id-to-path id 'invoice) t))

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
(defun org-contact-complete-link () (narf--org-complete 'contact))
;;;###autoload
(defun org-project-complete-link () (narf--org-complete 'project))
;;;###autoload
(defun org-invoice-complete-link () (narf--org-complete 'invoice))

(defun narf--org-crm-assert-type (type)
  (unless (memq type '(project contact invoice))
    (user-error "Not a valid type: %s" type)))

(defun narf--org-crm-new-path (name type)
  (narf--org-crm-assert-type type)
  (let* ((prefix
          (replace-regexp-in-string
           "/+$" "" (symbol-value (intern (format "org-directory-%ss" type)))))
         (last-file (car-safe (sort (f-glob "*.org" prefix) 'string>))))
    (when last-file
      (let* ((old-id (narf--org-crm-path-to-id last-file type))
             (new-id (format "%04X" (1+ old-id))))
        (if (eq type 'invoice)
            (format "%s/%s-%s.org" prefix (format-time-string "%y%m") new-id)
          (format "%s/%s-%s.org" prefix
                  new-id (replace-regexp-in-string "[][ !@#$%^&*()]" "-" name)))))))

(defun narf--org-crm-path-to-id (path type)
  (narf--org-crm-assert-type type)
  (let ((base (f-filename path)))
    (string-to-number
     (if (eq type 'invoice)
         (substring base (1+ (string-match-p "-" base)) (string-match-p ".org" base))
       (substring base 0 (string-match-p "[-.]" base)))
      16)))

(defun narf--org-crm-id-to-path (id type)
  (narf--org-crm-assert-type type)
  (let* ((prefix
          (replace-regexp-in-string
           "/+$" "" (symbol-value (intern (format "org-directory-%ss" type))))))
    (car-safe
     (f-glob (format (if (eq type 'invoice) "*-%04X.org" "%04X*.org")
                     (string-to-number id 16))
             prefix))))

(defun narf--org-crm (&optional id type new-p)
  (let ((file (if new-p
                  (or (narf--org-crm-new-path id type)
                      (user-error "path could not be resolved: type(%s) name(%s)" type name))
                (or (narf--org-crm-id-to-path id type)
                    (user-error "id %s could not be resolved in %s" id type))))
        (old-buffer (current-buffer)))
    (find-file file)
    (with-current-buffer old-buffer
      (when (evil-visual-state-p)
        (org-insert-link
         nil (format "%s:%s" (symbol-name type) (narf--org-crm-path-to-id file type))
         (buffer-substring-no-properties (region-beginning) (region-end)))))))


;;;###autoload (autoload 'narf:org-crm-project "defuns-org-crm" nil t)
(evil-define-command narf:org-crm-project (&optional bang name)
  (interactive "<!><a>")
  (if bang
      (narf--org-crm name 'project t)
    (narf/helm-org-crm-projects)))
;;;###autoload (autoload 'narf:org-crm-contact "defuns-org-crm" nil t)
(evil-define-command narf:org-crm-contact (&optional bang name)
  (interactive "<!><a>")
  (if bang
      (narf--org-crm name 'contact t)
    (narf/helm-org-crm-contacts)))
;;;###autoload (autoload 'narf:org-crm-invoice "defuns-org-crm" nil t)
(evil-define-command narf:org-crm-invoice (&optional bang)
  (interactive "<!>")
  (if bang
      (narf--org-crm nil 'invoice t)
    (narf/helm-org-crm-invoices)))


(provide 'defuns-org-crm)
;;; defuns-org-crm.el ends here
