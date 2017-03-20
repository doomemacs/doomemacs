;;; ../core/autoload/bootstrap.el

(defvar doom-bootstraps nil
  "TODO")

;;;###autoload
(defmacro def-bootstrap! (name &rest forms)
  (declare (indent defun))
  `(push (cons ',name
               (lambda ()
                 (cl-flet ((sh (lambda (&rest args) (apply 'doom-sh args)))
                           (sh& (lambda (&rest args) (apply 'doom-async-sh args)))
                           (sudo (lambda (&rest args) (apply 'doom-sudo args)))
                           (fetch (lambda (&rest args) (apply 'doom-fetch args)))
                           (message (lambda (&rest args)
                                      (apply 'message (format "[%s] %s" ,(symbol-name name) (car args))
                                             (cdr args)))))
                   (with-demoted-errors "BOOTSTRAP ERROR: %s"
                     ,@forms))))
         doom-bootstraps))

;;;###autoload
(defun doom/bootstrap (ids)
  "Bootstraps a module, if it has a bootstrapper. Bootstraps are expected to be
recipes for setting up the external dependencies of a module by, for instance,
using the OS package manager to install them, or retrieving them from a repo
using `doom-fetch'."
  (interactive
   (list (list (completing-read "Bootstrap: " (mapcar 'car doom-bootstraps) nil t))))
  (doom-initialize-packages t)
  ;; Error out if any of the bootstraps don't exist or aren't valid functions.
  ;; If something goes wrong, it's likely we don't want to continue.
  (let ((err-not-found (cl-remove-if (lambda (id) (assq id doom-bootstraps)) ids))
        (err-not-func  (cl-remove-if (lambda (id) (functionp (cdr (assq id doom-bootstraps)))) ids)))
    (when (or (and err-not-found
                   (message "ERROR: These bootstraps don't exist: %s" err-not-found))
              (and err-not-func
                   (message "ERROR: These bootstraps were invalid: %s" err-not-func)))
      (error "There were errors. Aborting.")))
  (dolist (id ids)
    (let ((bootstrap (assq id doom-bootstraps)))
      (message "[%s] BOOTSTRAP START" id)
      (with-demoted-errors (format "[%s] ERROR: %%s" id)
        (unless (funcall (cdr bootstrap))
          (message "[%s] DONE (already bootstrapped)" id))))))

