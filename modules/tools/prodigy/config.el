;;; tools/prodigy/config.el -*- lexical-binding: t; -*-

(after! prodigy
  (defadvice! +prodigy--add-project-property-a (orig-fn &rest args)
    "Adds a new :project property to prodigy services, which hides the service
unless invoked from the relevant project."
    :around #'prodigy-services
    (let ((project-root (downcase (or (doom-project-root) default-directory)))
          (services (apply orig-fn args)))
      (if current-prefix-arg
          services
        (cl-remove-if-not (lambda (service)
                            (let ((project (plist-get service :project)))
                              (or (not project)
                                  (file-in-directory-p project-root project))))
                          services))))

  (define-key prodigy-mode-map "d" #'+prodigy/delete))

