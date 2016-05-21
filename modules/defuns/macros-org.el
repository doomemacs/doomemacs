;;; macros-org.el

;;;###autoload
(defmacro define-org-section! (type directory &optional id-func link-glob)
  (setq directory (f-slash directory))
  (let* ((type-str (symbol-name type))
         (link-sym (intern (format "doom/org-link-%s" type-str)))
         (dir-var (intern (format "org-directory-%s" type-str))))
    `(progn
       ;; Variable containing directory: `org-directory-%s'
       (defvar ,dir-var ,(expand-file-name directory org-directory))

       ;; Open helm in directory: `doom/helm-org-%s'
       (evil-define-command ,(intern (format "doom:org-search-%s" type-str)) ()
         (interactive)
         (let ((default-directory (concat ,dir-var "/" (format-time-string "%Y") "/")))
           (helm-find-files nil)))

       ;; Open helm in directory: `doom/helm-org-%s'
       (defun ,(intern (format "doom/helm-org-find-file-in-%s" type-str)) ()
         (interactive)
         (helm-do-ag (f-slash ,dir-var)))

       ;; Org link handler
       (defun ,link-sym (id)
         (let ((path (f-glob (format (or ,link-glob "%s.org") id) ,dir-var)))
           (unless path
             (error "ID not found: %s" id))
           (org-open-file (car path) t)))
       (org-add-link-type ,type-str ',link-sym)

       ;; Org completion handler
       (defun ,(intern (format "org-%s-complete-link" type-str)) ()
         (let* ((default-directory (f-slash ,dir-var))
                (file (org-iread-file-name "> "))
                (relpath (f-relative file ,dir-var)))
           (when (and (not (file-exists-p file))
                      (y-or-n-p (format "Create %s?" relpath)))
             (write-region "" nil file)
             (message "Created %s" file))
           (format "%s:%s" ,type-str ,(if id-func `(funcall ,id-func relpath) '(f-no-ext relpath)))
           )))))

(provide 'macros-org)
;;; macros-org.el ends here
