;;; tools/prodigy/autoload.el -*- lexical-binding: t; -*-

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
   (when-let (service (prodigy-service-at-pos))
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
