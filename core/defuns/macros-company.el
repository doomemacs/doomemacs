;;; macros-company.el --- macros for company-mode
;; for ../core-company.el

;;;###autoload
(defmacro define-company-backend! (hook backends)
  "Register a company backend for a mode."
  (let ((def-name (intern (format "narf--init-company-%s" hook)))
        (quoted (eq (car-safe backends) 'quote)))
    `(progn
       (defun ,def-name ()
         (set (make-local-variable 'company-backends)
              (append '((,@(mapcar (lambda (backend)
                                     (if quoted
                                         backend
                                       (intern (format "company-%s" backend))))
                                   (if quoted (cadr backends) backends))))
                      company-backends)))
       (add-hook ',(intern (format "%s-hook" hook)) ',def-name))))

(provide 'macros-company)
;;; macros-company.el ends here
