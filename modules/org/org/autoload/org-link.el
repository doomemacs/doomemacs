;;; org/org/autoload/org-link.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-link-read-file (key dir)
  (let ((file (read-file-name (format "%s: " (capitalize key)) dir)))
    (format "%s:%s"
            key
            (file-relative-name file dir))))

;;;###autoload
(defun +org-link-read-directory (key dir)
  (let ((file (read-directory-name (format "%s: " (capitalize key)) dir)))
    (format "%s:%s"
            key
            (file-relative-name file dir))))
