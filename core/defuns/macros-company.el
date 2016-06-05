;;; macros-company.el --- macros for company-mode
;; for ../core-company.el

;;;###autoload
(defmacro def-company-backend! (hooks backends)
  "Register a company backend for a mode."
  (let* ((hooks (if (listp hooks) hooks (list hooks)))
         (def-name (intern (format "doom--init-company-%s"
                                   (mapconcat 'identity (mapcar 'symbol-name hooks) "-"))))
         (quoted (eq (car-safe backends) 'quote)))
    `(progn
       (defun ,def-name ()
         (require 'company)
         (set (make-local-variable 'company-backends)
              (append '((,@(mapcar (lambda (backend)
                                     (if quoted
                                         backend
                                       (intern (format "company-%s" backend))))
                                   (if quoted (cadr backends) backends))))
                      company-backends)))
       (add-hook! ,hooks ',def-name))))

(provide 'macros-company)
;;; macros-company.el ends here
