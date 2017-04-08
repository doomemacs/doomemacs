;;; lang/org/autoload/attach.el

(defun +org--attach-icon (path)
  (char-to-string (pcase (downcase (file-name-extension path))
                    ("jpg" ?) ("jpeg" ?) ("png" ?) ("gif" ?)
                    ("pdf" ?)
                    ("ppt" ?) ("pptx" ?)
                    ("xls" ?) ("xlsx" ?)
                    ("doc" ?) ("docx" ?)
                    ("ogg" ?) ("mp3" ?) ("wav" ?)
                    ("mp4" ?) ("mov" ?) ("avi" ?)
                    ("zip" ?) ("gz" ?) ("tar" ?) ("7z" ?) ("rar" ?)
                    (_ ?))))

;;;###autoload
(defun +org-cleanup-attachments ()
  ;; "Deletes any attachments that are no longer present in the org-mode buffer."
  (let* ((attachments-local (+org-attachments))
         (attachments (directory-files org-attach-directory t "^[^.]" t))
         (to-delete (cl-set-difference attachments-local attachments)))
    ;; TODO
    to-delete))

(defun +org-attachments ()
  (unless (eq major-mode 'org-mode)
    (user-error "Not an org buffer"))
  (org-save-outline-visibility nil
    (let ((attachments '())
          element
          file)
      (when (and (file-directory-p org-attach-directory)
                 (> (length (file-expand-wildcards (expand-file-name "*" org-attach-directory))) 0))
        (save-excursion
          (goto-char (point-min))
          (while (progn (org-next-link) (not org-link-search-failed))
            (setq element (org-element-context))
            (when-let (file (and (eq (org-element-type element) 'link)
                                 (expand-file-name (org-element-property :path element))))
              (when (and (string= (org-element-property :type element) "file")
                         (string= (concat (file-name-base (directory-file-name (file-name-directory file))) "/")
                                  org-attach-directory)
                         (file-exists-p file))
                (push file attachments))))))
      (cl-remove-duplicates attachments))))

;;;###autoload
(defun +org-download-dnd (uri action)
  (if (eq major-mode 'org-mode)
      (doom:org-attach uri) ;; FIXME
    (let ((dnd-protocol-alist
           (rassq-delete-all '+org-download-dnd (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))

