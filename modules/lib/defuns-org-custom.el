;;; defuns-org-custom.el -- custom functions, links, etc. for Org-mode

(progn ;; Custom links
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

  (defun narf-org-complete (type)
    (let ((default-directory (symbol-value (intern (format "org-directory-%ss" type)))))
      (let* ((file (org-iread-file-name ">>> "))
             (match (s-match "^\\([0-9]+\\)[-.]" (f-filename file))))
        (unless match
          (user-error "Invalid file ID"))
        (format "%s:%s" type (cadr match)))))

  (defun org-contact-complete-link ()
    (narf-org-complete "contact"))
  (defun org-project-complete-link ()
    (narf-org-complete "project"))
  (defun org-invoice-complete-link ()
    (narf-org-complete "invoice")))

(progn ;; Deft
;;;###autoload
  (defun narf/deft-projects ()
    (interactive)
    (require 'deft)
    (let ((deft-directory org-directory-projects))
      (deft)))

;;;###autoload
  (defun narf/deft-contact ()
    (interactive)
    (require 'deft)
    (let ((deft-directory org-directory-contacts))
      (deft)))

;;;###autoload
  (defun narf/deft-invoices ())
;;;###autoload
  (defun narf/deft-writing ()))

(provide 'defuns-org-custom)
;;; defuns-org-custom.el ends here
