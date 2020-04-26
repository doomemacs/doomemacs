;;; lang/org/autoload/org-link.el -*- lexical-binding: t; -*-

(defun +org--relative-path (path root)
  (if (and buffer-file-name (file-in-directory-p buffer-file-name root))
      (file-relative-name path)
    path))

(defun +org--read-link-path (key dir &optional fn)
  (let ((file (funcall (or fn #'read-file-name) (format "%s: " (capitalize key)) dir)))
    (format "%s:%s" key (file-relative-name file dir))))

;;;###autoload
(defun +org-define-basic-link (key dir-var &rest plist)
  "Define a link with some basic completion & fontification.

KEY is the name of the link type. DIR-VAR is the directory variable to resolve
links relative to. PLIST is passed to `org-link-set-parameters' verbatim.

Links defined with this will be rendered in the `error' face if the file doesn't
exist, and `org-link' otherwise."
  (declare (indent 2))
  (let ((requires (plist-get plist :requires))
        (dir-fn (if (functionp dir-var)
                    dir-var
                  (lambda () (symbol-value dir-var)))))
    (apply #'org-link-set-parameters
           key
           :complete (lambda ()
                       (if requires (mapc #'require (doom-enlist requires)))
                       (+org--relative-path (+org--read-link-path key (funcall dir-fn))
                                            (funcall dir-fn)))
           :follow   (lambda (link)
                       (org-link-open-as-file (expand-file-name link (funcall dir-fn)) nil))
           :face     (lambda (link)
                       (if (file-exists-p (expand-file-name link (funcall dir-fn)))
                           'org-link
                         'error))
           (doom-plist-delete plist :requires))))


;;
;;; Image data functions (for custom inline images)

;;;###autoload
(defun +org-image-file-data-fn (protocol link _description)
  "Intepret LINK as an image file path and return its data."
  (setq
   link (expand-file-name
         link
         (pcase protocol
           ("download" (or org-download-image-dir org-attach-id-dir default-directory))
           ("attachment" org-attach-id-dir)
           (_ default-directory))))
  (when (and (file-exists-p link)
             (image-type-from-file-name link))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq buffer-file-coding-system 'binary)
      (insert-file-contents-literally link)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun +org-inline-image-data-fn (_protocol link _description)
  "Interpret LINK as base64-encoded image data."
  (base64-decode-string link))

;;;###autoload
(defun +org-http-image-data-fn (protocol link _description)
  "Interpret LINK as an URL to an image file."
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
