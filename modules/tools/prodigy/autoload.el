;;; tools/prodigy/autoload.el -*- lexical-binding: t; -*-

;; FIXME obsolete :service
;;;###autoload
(def-setting! :service (&rest plist)
  "TODO"
  `(after! prodigy
     (prodigy-define-service ,@plist)))

;;;###autoload
(defun +prodigy/create ()
  "Interactively create a new prodigy service."
  (interactive)
  ;; TODO
  )

;;;###autoload
(defun +prodigy/delete (arg)
  "Delete service at point. Asks for confirmation."
  (interactive "P")
  (prodigy-with-refresh
   (when-let* ((service (prodigy-service-at-pos)))
     (let ((name (plist-get service :name)))
       (cond ((or arg
                  (y-or-n-p (format "Delete '%s' service?" name)))
              (setq prodigy-services (delete service prodigy-services))
              (ignore-errors
                (prodigy-goto-next-line))
              (message "Successfully deleted service: %s" name))
             (t
              (message "Aborted")))))))

;;;###autoload
(defun +prodigy/cleanup ()
  "Delete all services associated with projects that don't exist."
  (interactive)
  (cl-loop for service in prodigy-services
           if (and (plist-member service :project)
                   (file-directory-p (plist-get service :project)))
           collect service into services
           finally do (setq prodigy-service services)))

;;;###autoload
(defun +prodigy*services (orig-fn &rest args)
  "Adds a new :project property to prodigy services, which hides the service
unless invoked from the relevant project."
  (let ((project-root (downcase (or (doom-project-root) default-directory)))
        (services (apply orig-fn args)))
    (if current-prefix-arg
        services
      (cl-remove-if-not (lambda (service)
                          (let ((project (plist-get service :project)))
                            (or (not project)
                                (file-in-directory-p project-root project))))
                        services))))
