;;; defuns-org-notebook.el

;; Keep track of attachments
(defvar narf-org-attachments-list '() "A list of attachments for the current buffer")
(make-variable-buffer-local 'narf-org-attachments-list)

;;;###autoload
(defun narf/org-start ()
  (interactive)
  (let ((wg (wg-get-workgroup "*ORG*" t))
        orig-win)
    (ignore-errors
      (if (eq wg (wg-current-workgroup t))
          (wg-switch-to-workgroup wg)
        (narf:workgroup-new nil "*ORG*" t)))
    (setq orig-win (selected-window))
    (find-file (expand-file-name "Todo.org" org-directory))
    (narf/neotree)
    (select-window orig-win t)))

;;;###autoload
(defun narf/org-notebook-new ()
  (interactive)
  (projectile-invalidate-cache nil)
  (let* ((default-directory org-directory)
         (dir (projectile-complete-dir))
         (narf-org-quicknote-dir dir))
    (when dir
      (narf/org-notebook-quick-note))))

;;;###autoload
(defun narf/org-notebook-quick-note ()
  (interactive)
  (let (text)
    (when (evil-visual-state-p)
      (setq text (buffer-substring-no-properties evil-visual-beginning evil-visual-end)))
    (switch-to-buffer (generate-new-buffer "*quick-note*"))
    (setq default-directory narf-org-quicknote-dir)
    (erase-buffer)
    (insert text)))

;;;###autoload
(defun narf/org-download-dnd (uri action)
  (if (eq major-mode 'org-mode)
      (narf:org-attach uri)
    (let ((dnd-protocol-alist
           (rassq-delete-all 'narf/org-download-dnd (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))

;;;###autoload (autoload 'narf:org-attach "defuns-org-notebook" nil t)
(evil-define-command narf:org-attach (&optional uri)
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
                                     " " (narf/org-attach-icon rel-path)))

          (insert (if image-p
                      (format "[[./%s]]" rel-path)
                    (format "%s [[./%s][%s]]"
                            (narf/org-attach-icon rel-path)
                            rel-path (f-filename rel-path)))))
        (when (string-match-p (regexp-opt '("jpg" "jpeg" "gif" "png")) (f-ext rel-path))
          (org-toggle-inline-images)))
    (let ((attachments (narf-org-attachments)))
      (unless attachments
        (user-error "No attachments in this file"))
      (helm :sources (helm-build-sync-source "Attachments" :candidates attachments)))))

;;;###autoload
(defun narf/org-attach-icon (path)
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
(defun narf/org-attachments ()
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
(defun narf/org-cleanup-attachments ()
  "Deletes any attachments that are no longer present in the org-mode buffer."
  (let* ((attachments (narf/org-attachments))
         (to-delete (-difference narf-org-attachments-list attachments)))
    (mapc (lambda (f)
            (message "Deleting attachment: %s" f)
            (delete-file f t))
          to-delete)
    (setq narf-org-attachments-list attachments)))


;;
;; Easy searching
;;

;; Ex-mode interface for `helm-ag'. If `bang', then `search' is interpreted as
;; regexp.
;;;###autoload (autoload 'narf:org-helm-search "defuns-org-notebook" nil t)
(evil-define-operator narf:org-helm-search (beg end &optional search bang)
  (interactive "<r><a><!>")
  (narf:helm-ag-search beg end (if bang (concat "^\\*+.*" search ".*$") search) t org-directory))

(provide 'defuns-org-notebook)
;;; defuns-org-notebook.el ends here
