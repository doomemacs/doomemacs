;;; macros-org.el

;;;###autoload
(defmacro define-org-link! (type directory &optional id-func)
  (setq directory (f-slash directory))
  (let* ((type-str (symbol-name type))
         (link-sym (intern (format "narf/org-link-%s" type-str)))
         (dir-var (intern (format "org-directory-%s" type-str))))
    `(progn
       (defvar ,dir-var ,(expand-file-name directory org-directory))

       (defun ,(intern (format "narf/helm-org-%s" type-str)) ()
         (interactive)
         (let ((default-directory ,directory))
           (helm-deft)))

       (defun ,link-sym (id)
         (let ((path (f-glob (format "%s*.org" id) ,directory)))
           (unless path
             (error "ID not found: %s" id))
           (org-open-file (car path) t)))
       (org-add-link-type ,type-str ',link-sym)

       (defun ,(intern (format "narf/org-%s-at-pt" type-str)) ()
         (interactive)
         (let* ((id (or (narf/org-get-property ,type-str)
                        (thing-at-point 'word t)))
                (path (f-glob (format "%s*.org" id) ,dir-var)))
           (unless path
             (user-error "Couldn't find anything with %s (%s in %s)" id path ,directory))
           (org-open-file (car path) t)))

       (defun ,(intern (format "org-%s-complete-link" type-str)) ()
         (let* ((default-directory (f-slash ,dir-var))
                (file (org-iread-file-name ">>> "))
                (relpath (f-relative file ,dir-var)))
           (when (and (not (file-exists-p file))
                      (y-or-n-p (format "Create %s?" relpath)))
             (write-region "" nil file)
             (message "Created %s" file))
           (format "%s:%s" ,type-str ,(if id-func `(funcall ,id-func relpath) 'relpath))
           )))))

(provide 'macros-org)
;;; macros-org.el ends here
