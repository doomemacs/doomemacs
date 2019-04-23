;;; lang/org/autoload/org-attach.el -*- lexical-binding: t; -*-
;;;###if (featurep! +attach)

(defun +org-attach--icon (path)
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


;;
(defvar +org-attachments nil
  "A list of all indexed attachments in `org-directory'.")

(defvar +org-attachments-files nil
  "A list of all attachments in `org-attach-directory'.")

(defun +org-attachments--list (&optional beg end)
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
                                        org-attach-directory)))
           (when (and (equal "file" (org-element-property :type context))
                      (file-in-directory-p link org-attach-directory))
             (push (file-name-nondirectory link) attachments))))))
    (cl-delete-duplicates attachments :test #'string=)))

;;;###autoload
(defun +org-attach/sync (arg)
  "Reindex all attachments in `org-directory' and delete orphaned attachments in
`org-attach-directory'. If ARG (universal arg), conduct a dry run."
  (declare (interactive-only t))
  (interactive "P")
  (message "Reloading")
  (setq +org-attachments-files (directory-files org-attach-directory nil "^[^.]" t))
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (dolist (org-file (directory-files-recursively org-directory "\\.org$"))
      (insert-file-contents-literally org-file))
    (setq +org-attachments (+org-attachments--list)))
  ;; clean up
  (let ((deleted 0))
    (dolist (file (cl-set-difference +org-attachments-files +org-attachments
                                     :test #'string=))
      (message "Deleting orphaned attachment: %s" file)
      (cl-incf deleted)
      (unless arg
        (delete-file (expand-file-name file org-attach-directory))))
    (message "Buffer's attachments synced (%d deleted)" deleted)))

;;;###autoload
(defun +org-attach/find-file ()
  "Open a file from `org-attach-directory'."
  (interactive)
  (doom-project-browse org-attach-directory))

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

;;;###autoload
(defun +org-attach-download-dnd (uri action)
  "TODO"
  (if (eq major-mode 'org-mode)
      (+org-attach/uri uri)
    (let ((dnd-protocol-alist
           (rassq-delete-all '+org-attach-download-dnd
                             (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))


;;
;; Advice

;;;###autoload
(defun +org-attach*insert-link (_link filename)
  "Produces and inserts a link to FILENAME into the document.

If FILENAME is an image, produce an attach:%s path, otherwise use file:%s (with
an file icon produced by `+org-attach--icon')."
  (if (looking-back "^[ \t]+" (line-beginning-position))
      (delete-region (match-beginning 0) (match-end 0))
    (newline))
  (cond ((image-type-from-file-name filename)
         (insert
          (concat (if (= org-download-image-html-width 0) ""
                    (format "#+attr_html: :width %dpx\n" org-download-image-html-width))
                  (if (= org-download-image-latex-width 0) ""
                    (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width))
                  (cond ((file-in-directory-p filename org-attach-directory)
                         (format "[[attach:%s]]" (file-relative-name filename org-attach-directory)))
                        ((file-in-directory-p filename org-directory)
                         (format org-download-link-format (file-relative-name filename org-directory)))
                        (t
                         (format org-download-link-format filename)))))
         (org-display-inline-images))
        (t
         (insert
          (format "%s [[./%s][%s]] "
                  (+org-attach--icon filename)
                  (file-relative-name filename (file-name-directory buffer-file-name))
                  (file-name-nondirectory (directory-file-name filename)))))))

