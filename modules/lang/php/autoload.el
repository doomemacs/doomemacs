;;; lang/php/autoload.el -*- lexical-binding: t; -*-

(defvar +php-composer-conf (make-hash-table :test 'equal))

;;;###autoload
(defun +php-composer-conf (&optional project-root refresh-p)
  "Retrieve the contents of composer.json as an alist. If REFRESH-P is non-nil
ignore the cache."
  (let ((project-root (or project-root (doom-project-root))))
    (or (and (not refresh-p) (gethash project-root +php-composer-conf))
        (let ((package-file (expand-file-name "composer.json" project-root)))
          (when-let* ((data (and (file-exists-p package-file)
                                 (require 'json)
                                 (json-read-file package-file))))
            (puthash project-root data +php-composer-conf))))))

;;;###autoload
(defun +php|init-ac-php-core-eldoc ()
  "Initialize eldoc support for `php-mode' with `ac-php-core'. Fails gracefully
if phpctags isn't installed."
  (require 'company-php)
  (if (file-exists-p ac-php-ctags-executable)
      (ac-php-core-eldoc-setup)
    (message "phpctags is missing; eldoc support is disabled")))
