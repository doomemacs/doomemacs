;;; lang/php/autoload.el -*- lexical-binding: t; -*-

(defvar +php-composer-conf (make-hash-table :test 'equal))

;;;###autoload
(defun +php-company-backend (command &optional arg &rest _ignored)
  "A delegating company-backend that uses `company-phpactor' if phpactor is
available and installed, or `php-extras-company' otherwise."
  (cond ((and (require 'company-phpactor nil t)
              (ignore-errors (phpactor-find-executable)))
         (company-phpactor command arg))
        ((and (require 'php-extras nil t)
              (file-exists-p (concat php-extras-eldoc-functions-file ".el")))
         (php-extras-company command arg))))

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
  (require 'ac-php-core)
  (cond ((not ac-php-ctags-executable))
        ((not (file-exists-p ac-php-ctags-executable))
         (message "Could not find phpctags executable, eldoc support is disabled")
         (message "To disable these warnings, set ac-php-ctags-executable to nil"))
        ((ac-php-core-eldoc-setup))))
