;;; defuns-org-notebook.el

;;;###autoload
(defun doom/org ()
  (interactive)
  (find-file (f-expand "inbox.org" org-directory)))

;;;###autoload
(defun doom/org-notebook-new ()
  (interactive)
  (projectile-invalidate-cache nil)
  (let* ((default-directory org-directory)
         (dir (projectile-complete-dir))
         (doom-org-quicknote-dir dir))
    (when dir
      (doom/org-notebook-quick-note))))

;;;###autoload
(defun doom/org-notebook-quick-note ()
  (interactive)
  (let (text)
    (when (evil-visual-state-p)
      (setq text (buffer-substring-no-properties evil-visual-beginning evil-visual-end)))
    (switch-to-buffer (generate-new-buffer "*quick-note*"))
    (setq default-directory doom-org-quicknote-dir)
    (erase-buffer)
    (insert text)))

;;;###autoload
(defun doom/org-download-dnd (uri action)
  (if (eq major-mode 'org-mode)
      (doom:org-attach uri)
    (let ((dnd-protocol-alist
           (rassq-delete-all 'doom/org-download-dnd (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))

;;;###autoload (autoload 'doom:org-attach "defuns-org-notebook" nil t)
(evil-define-command doom:org-attach (&optional uri)
  (interactive "<a>")
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (if uri
      (let* ((rel-path (org-download--fullname uri))
             (new-path (f-expand rel-path))
             (image-p (image-type-from-file-name uri)))
        (cond ((string-match-p (concat "^" (regexp-opt '("http" "https" "nfs" "ftp" "file")) ":/") uri)
               (url-copy-file uri new-path))
              (t (copy-file uri new-path)))
        (unless new-path
          (user-error "No file was provided"))
        (if (evil-visual-state-p)
            (org-insert-link nil (format "./%s" rel-path)
                             (concat (buffer-substring-no-properties (region-beginning) (region-end))
                                     " " (doom/org-attach-icon rel-path)))

          (insert (if image-p
                      (format "[[./%s]]" rel-path)
                    (format "%s [[./%s][%s]]"
                            (doom/org-attach-icon rel-path)
                            rel-path (f-filename rel-path)))))
        (when (string-match-p (regexp-opt '("jpg" "jpeg" "gif" "png")) (f-ext rel-path))
          (org-toggle-inline-images)))
    (let ((attachments (doom-org-attachments)))
      (unless attachments
        (user-error "No attachments in this file"))
      (helm :sources (helm-build-sync-source "Attachments" :candidates attachments)))))

;;;###autoload
(defun doom/org-attach-icon (path)
  (char-to-string (pcase (downcase (f-ext path))
                    ("jpg" ?) ("jpeg" ?) ("png" ?) ("gif" ?)
                    ("pdf" ?)
                    ("ppt" ?) ("pptx" ?)
                    ("xls" ?) ("xlsx" ?)
                    ("doc" ?) ("docx" ?)
                    ("ogg" ?) ("mp3" ?) ("wav" ?)
                    ("mp4" ?) ("mov" ?) ("avi" ?)
                    ("zip" ?) ("gz" ?) ("tar" ?) ("7z" ?) ("rar" ?)
                    (t ?))))

;;;###autoload
(defun doom/org-attachments ()
  "Retrieves a list of all the attachments pertinent to the currect org-mode buffer."
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
(defun doom/org-cleanup-attachments ()
  "Deletes any attachments that are no longer present in the org-mode buffer."
  (let* ((attachments (doom/org-attachments))
         (to-delete (-difference doom-org-attachments-list attachments)))
    (mapc (lambda (f)
            (message "Deleting attachment: %s" f)
            (delete-file f t))
          to-delete)
    (setq doom-org-attachments-list attachments)))


;;
;; Easy searching
;;

;; Ex-mode interface for `helm-ag'. If `bang', then `search' is interpreted as
;; regexp.
;;;###autoload (autoload 'doom:org-helm-search "defuns-org-notebook" nil t)
(evil-define-operator doom:org-helm-search (beg end &optional search bang)
  (interactive "<r><a><!>")
  (doom:helm-ag-search beg end (if bang (concat "^\\*+.*" search ".*$") search) t org-directory))

(provide 'defuns-org-notebook)
;;; defuns-org-notebook.el ends here
