;;; lang/org/autoload/org-link.el -*- lexical-binding: t; -*-

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

;;;###autoload
(defun +org-inline-data-image (_protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (base64-decode-string link))

;;;###autoload
(defun +org-image-link (protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (when (image-type-from-file-name link)
    (if-let* ((buf (url-retrieve-synchronously (concat protocol ":" link))))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n" nil t)
          (buffer-substring-no-properties (point) (point-max)))
      (message "Download of image \"%s\" failed" link)
      nil)))


;;
;;; Commands

;;;###autoload
(defun +org/remove-link ()
  "Unlink the text at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((label (if (match-end 2)
                     (match-string-no-properties 2)
                   (org-link-unescape (match-string-no-properties 1)))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert label))))
