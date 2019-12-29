;;; lang/org/autoload/org-attach.el -*- lexical-binding: t; -*-

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
(defun +org/open-gallery-from-attachments ()
  "TODO"
  (interactive)
  (require 'org-attach)
  (if-let (dir (org-attach-dir))
      (pop-to-buffer
       ;; Rather than opening dired *and* image-dired windows, suppress them
       ;; both and open only the image-dired window.
       (save-window-excursion
         (image-dired dir)
         (current-buffer)))
    (user-error "No attachments for this node")))

;;;###autoload
(defun +org/find-file-in-attachments ()
  "Open a file from `org-attach-id-dir'."
  (interactive)
  (doom-project-browse org-attach-id-dir))

;;;###autoload
(defun +org/attach-file-and-insert-link (path)
  "Downloads the file at PATH and insert an org link at point.
PATH (a string) can be an url, a local file path, or a base64 encoded datauri."
  (interactive "sUri/file: ")
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an org buffer"))
  (require 'org-download)
  (condition-case ex
      (let ((raw-uri (url-unhex-string path)))
        (cond ((string-match-p "^data:image/png;base64," path)
               (org-download-dnd-base64 path nil))
              ((image-type-from-file-name raw-uri)
               (org-download-image raw-uri))
              ((let ((new-path (expand-file-name (org-download--fullname raw-uri))))
                 ;; Download the file
                 (if (string-match-p (concat "^" (regexp-opt '("http" "https" "nfs" "ftp" "file")) ":/") path)
                     (url-copy-file raw-uri new-path)
                   (copy-file path new-path))
                 ;; insert the link
                 (org-download-insert-link raw-uri new-path)))))
    (error
     (user-error "Failed to attach file: %s" (error-message-string ex)))))
