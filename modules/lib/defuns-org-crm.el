;;; defuns-org-crm.el --- for my custom org-based CRM

;;; Personal CRM
;; (defvar narf--helm-org-cache '())
(defvar narf--helm-org-files '())

(defvar narf--helm-org-title "Org files")
(defvar narf--helm-org-dir org-directory)
(defvar narf--helm-org-params '(created contact email country issued paid))

(defun narf--helm-org-init ()
  (setq narf--helm-org-files
        (mapcar 'narf--helm-org-metadata
                (f-entries narf--helm-org-dir (lambda (f) (and (f-ext? f "org") (> (f-size f) 0))) t))))

(defun narf--helm-org-metadata (file &optional params)
  (let ((params (or params narf--helm-org-params))
        (base (f-base file))
        alist content title)
    (with-temp-buffer
      (insert-file-contents file nil nil nil t)
      (setq content (concat (buffer-string))))
    (setq title (let ((title (deft-parse-title file content)))
                  (if (string= title "")
                      "-"
                    title)))
    (setq alist
          (list file
                (cons 'id (substring base 0 (string-match "-" base)))
                (cons 'path file)
                (cons 'title title)
                (cons 'summary (truncate-string-to-width
                                (replace-regexp-in-string
                                 "[\n\t]" " "
                                 (if title
                                     (if (string-match (regexp-quote "#+end_src") content)
                                         (deft-chomp (substring content (match-end 0)
                                                                (string-match "^\\* " content (match-end 0))))
                                       "")
                                   content)
                                 content)
                                (window-width)))))
    (mapc (lambda (p)
            (let ((value (if (string-match (concat "^" (symbol-name p) ": +\\(.*\\)$") content)
                             (substring content (match-beginning 1) (match-end 1)))))
              (when value
                (add-to-list 'alist (cons p value) t))))
          params)
    alist))

;; TODO Make these defun names consistent!
(defun narf--helm-org-candidates ()
  narf--helm-org-files)
(defun narf--helm-org-real-to-display (alist)
  (format "[%s] [%s] %-20s -- (%s) %s"
          (cdr-safe (assoc 'id alist))
          (cdr-safe (assoc 'created alist))
          (cdr-safe (assoc 'title alist))
          (or (cdr-safe (assoc 'contact alist))
              (cdr-safe (assoc 'email alist))
              (cdr-safe (assoc 'country alist))
              "")
          (cdr-safe (assoc 'summary alist))))
(defun narf--helm-org-action (alist)
  (find-file (cdr-safe (assoc 'path alist))))

(defun narf--helm-org (&optional directory)
  (require 'deft)
  (let ((narf--helm-org-dir (or directory default-directory)))
    (helm :sources (helm-build-sync-source narf--helm-org-title
                     :init 'narf--helm-org-init
                     :candidates 'narf-helm-org-candidates
                     :real-to-display 'narf--helm-org-real-to-display
                     :action 'narf--helm-org-action)
          :buffer "*helm-deft*")))

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
(defun narf/helm-org-crm-writing ()
  (interactive)
  (let ((narf--helm-org-params '()))
    (narf--helm-org (expand-file-name "writing/" org-directory))))

;;; Custom links
(defun narf--org-crm-id-to-file (id dir &optional pattern)
  (let* ((glob (f-glob (format (concat "%s" (or pattern "%s-*.org")) dir id)))
         (glob-len (length glob)))
    (when (zerop glob-len)
      (user-error "Could not find file with that ID"))
    (car glob)))

;;;###autoload
(defun narf/org-crm-link-contact (id)
  (org-open-file (narf--org-crm-id-to-file id org-directory-contacts) t))
;;;###autoload
(defun narf/org-crm-link-project (id)
  (org-open-file (narf--org-crm-id-to-file id org-directory-projects) t))
;;;###autoload
(defun narf/org-crm-link-invoice (id)
  (org-open-file (narf--org-crm-id-to-file id org-directory-invoices "%s.org") t))

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

(defun narf--org-crm-delete (&optional directory)
  (let ((file (org-iread-file-name ">>> ")))
    (delete-file file)
    (message "Deleted %s" file)))

;;;###autoload
(defun narf/org-crm-contact-delete () (interactive) (narf--org-crm-delete org-directory-contacts))
;;;###autoload
(defun narf/org-crm-project-delete () (interactive) (narf--org-crm-delete org-directory-projects))
;;;###autoload
(defun narf/org-crm-invoice-delete () (interactive) (narf--org-crm-delete org-directory-invoices))

(provide 'defuns-org-crm)
;;; defuns-org-crm.el ends here
