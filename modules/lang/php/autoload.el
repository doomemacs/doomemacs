;;; lang/php/autoload.el -*- lexical-binding: t; -*-

(defvar +php-composer-conf (make-hash-table :test 'equal))

;;;###autoload
(defun +php-composer-conf (&optional project-root refresh-p)
  "Retrieve the contents of composer.json as an alist. If REFRESH-P is non-nil
ignore the cache."
  (let ((project-root (or project-root (doom-project-root))))
    (or (and (not refresh-p) (gethash project-root +php-composer-conf))
        (let ((package-file (expand-file-name "composer.json" project-root)))
          (when-let (data (and (file-exists-p package-file)
                               (require 'json)
                               (json-read-file package-file)))
            (puthash project-root data +php-composer-conf))))))

;;
;;; Commands


;;;###autoload
(defun +php/open-repl ()
  "Open PHP REPL."
  (interactive)
  (psysh)
  (pop-to-buffer (current-buffer)))
