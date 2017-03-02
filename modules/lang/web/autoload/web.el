;;; lang/web/autoload/web.el

(defvar +web-bower-conf (make-hash-table :test 'equal))

;;;###autoload
(defun +web-bower-conf (&optional project-root refresh-p)
  "Retrieves an alist of this project's 'bower.json'. If REFRESH-P is non-nil
ignore the cache."
  (let ((project-root (or project-root (doom-project-root))))
    (or (and (not refresh-p)
             (gethash project-root +web-bower-conf))
        (let ((package-file (expand-file-name "bower.json" project-root)))
          (when-let (json (and (file-exists-p package-file)
                               (json-read-file package-file)))
            (puthash project-root json +web-bower-conf))))))
