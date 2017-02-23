;;; lang/org/autoload/attach.el

(defun doom--org-attach-icon (path)
  (char-to-string (pcase (downcase (f-ext path))
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
         (attachments (f-entries org-attach-directory))
         (to-delete (-difference attachments-local attachments)))
    ;; TODO
    to-delete))

(defun +org-attachments ()
  (unless (eq major-mode 'org-mode)
    (user-error "Not an org buffer"))
  (org-save-outline-visibility nil
    (let ((attachments '())
          element
          file)
      (when (and (f-dir? org-attach-directory)
                 (> (length (f-glob (concat (f-slash org-attach-directory) "*"))) 0))
        (save-excursion
          (goto-char (point-min))
          (while (progn (org-next-link) (not org-link-search-failed))
            (setq element (org-element-lineage (org-element-context) '(link) t))
            (when element
              (setq file (expand-file-name (org-element-property :path element)))
              (when (and (string= (org-element-property :type element) "file")
                         (string= (concat (f-base (f-dirname file)) "/") org-attach-directory)
                         (file-exists-p file))
                (push file attachments))))))
      (-distinct attachments))))

;;;###autoload
(defun +org-download-dnd (uri action)
  (if (eq major-mode 'org-mode)
      (doom:org-attach uri) ;; FIXME
    (let ((dnd-protocol-alist
           (rassq-delete-all '+org-download-dnd (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))

