;;; lang/org/autoload/org-attach.el -*- lexical-binding: t; -*-

;;
(defvar +org-attachments nil
  "A list of all indexed attachments in `org-directory'.")

(defvar +org-attachments-files nil
  "A list of all attachments in `org-attach-id-dir'.")

(defun +org-list-attachments (&optional beg end)
  "Return a list of all attachment file names in the current buffer between BEG
and END (defaults to `point-min' and `point-max')."
  (let ((case-fold-search t)
        attachments)
    (or end (setq end (point-max)))
    (org-save-outline-visibility nil
      (org-with-wide-buffer
       (goto-char (or beg (point-min)))
       (while (search-forward "[[attach:" end t)
         (let* ((context (save-match-data (org-element-context)))
                (link (expand-file-name (org-link-unescape (org-element-property :path context))
                                        org-attach-id-dir)))
           (when (and (equal "file" (org-element-property :type context))
                      (file-in-directory-p link org-attach-id-dir))
             (push (file-name-nondirectory link) attachments))))))
    (cl-delete-duplicates attachments :test #'string=)))

;;;###autoload
(defun +org-attach-icon-for (path)
  (char-to-string
   (pcase (downcase (file-name-extension path))
     ((or "jpg" "jpeg" "png" "gif") ?)
     ("pdf" ?)
     ((or "ppt" "pptx") ?)
     ((or "xls" "xlsx") ?)
     ((or "doc" "docx") ?)
     ((or "ogg" "mp3" "wav" "aiff" "flac") ?)
     ((or "mp4" "mov" "avi") ?)
     ((or "zip" "gz" "tar" "7z" "rar") ?)
     (_ ?))))


;;;###autoload
(defun +org-attach/sync (arg)
  "Reindex all attachments in `org-directory' and delete orphaned attachments in
`org-attach-id-dir'. If ARG (universal arg), conduct a dry run."
  (declare (interactive-only t))
  (interactive "P")
  (message "Reloading")
  (setq +org-attachments-files (directory-files org-attach-id-dir nil "^[^.]" t))
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (dolist (org-file (directory-files-recursively org-directory "\\.org$"))
      (insert-file-contents-literally org-file))
    (setq +org-attachments (+org-list-attachments)))
  ;; clean up
  (let ((deleted 0))
    (dolist (file (cl-set-difference +org-attachments-files +org-attachments
                                     :test #'string=))
      (message "Deleting orphaned attachment: %s" file)
      (cl-incf deleted)
      (unless arg
        (delete-file (expand-file-name file org-attach-id-dir))))
    (message "Buffer's attachments synced (%d deleted)" deleted)))

;;;###autoload
(defun +org-attach/find-file ()
  "Open a file from `org-attach-id-dir'."
  (interactive)
  (doom-project-browse org-attach-id-dir))

;;;###autoload
(defun +org-attach/file (path)
  "Copies the file at PATH to `+org-attach-dir' and places an org link to it at
the cursor."
  (interactive "fAttach file: ")
  (+org-attach/uri path))

;;;###autoload
(defun +org-attach/uri (uri)
  "Downloads the file at URL and place an org link to it at the cursor."
  (interactive "sUri/file: ")
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an org buffer"))
  (require 'org-download)
  (let ((raw-uri (url-unhex-string uri)))
    (condition-case ex
        (cond ((string-match-p "^data:image/png;base64," uri)
               (org-download-dnd-base64 uri nil))
              ((image-type-from-file-name raw-uri)
               (org-download-image raw-uri))
              (t
               (let ((new-path (expand-file-name (org-download--fullname raw-uri))))
                 ;; Download the file
                 (if (string-match-p (concat "^" (regexp-opt '("http" "https" "nfs" "ftp" "file")) ":/") uri)
                     (url-copy-file raw-uri new-path)
                   (copy-file uri new-path))
                 ;; insert the link
                 (org-download-insert-link raw-uri new-path))))
      (error
       (user-error "Failed to attach file: %s" (error-message-string ex))))))
