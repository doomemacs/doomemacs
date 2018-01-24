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

;; (defun +org-attach-cleanup ()
;;   ;; "Deletes any attachments that are no longer present in the org-mode buffer."
;;   (let* ((attachments-local (+org-attachments))
;;          (attachments (directory-files org-attach-directory t "^[^.]" t))
;;          (to-delete (cl-set-difference attachments-local attachments)))
;;     ;; TODO
;;     to-delete))

;; (defun +org-attachments ()
;;   "List all attachments in the current buffer."
;;   (unless (eq major-mode 'org-mode)
;;     (user-error "Not an org buffer"))
;;   (org-save-outline-visibility nil
;;     (let ((attachments '())
;;           element)
;;       (when (and (file-directory-p org-attach-directory)
;;                  (> (length (file-expand-wildcards (expand-file-name "*" org-attach-directory))) 0))
;;         (save-excursion
;;           (goto-char (point-min))
;;           (while (progn (org-next-link) (not org-link-search-failed))
;;             (setq element (org-element-context))
;;             (when-let* (file (and (eq (org-element-type element) 'link)
;;                                   (expand-file-name (org-element-property :path element))))
;;               (when (and (string= (org-element-property :type element) "file")
;;                          (string= (concat (file-name-base (directory-file-name (file-name-directory file))) "/")
;;                                   org-attach-directory)
;;                          (file-exists-p file))
;;                 (push file attachments))))))
;;       (cl-remove-duplicates attachments))))

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
  (condition-case ex
      (cond ((string-match-p "^data:image/png;base64," uri)
             (org-download-dnd-base64 uri nil))
            ((image-type-from-file-name uri)
             (org-download-image uri))
            (t
             (let ((new-path (expand-file-name (org-download--fullname uri))))
               ;; Download the file
               (if (string-match-p (concat "^" (regexp-opt '("http" "https" "nfs" "ftp" "file")) ":/") uri)
                   (url-copy-file uri new-path)
                 (copy-file uri new-path))
               ;; insert the link
               (org-download-insert-link uri new-path))))
    (error
     (user-error "Failed to attach file: %s" (error-message-string ex)))))

;;;###autoload
(defun +org-attach-download-dnd (uri action)
  "TODO"
  (if (eq major-mode 'org-mode)
      (+org-attach/uri uri)
    (let ((dnd-protocol-alist
           (rassq-delete-all '+org-attach-download-dnd
                             (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))

;;;###autoload
(defun +org-attach*link-format (filename &optional ext)
  (format "%s%s.%s"
          (file-name-sans-extension filename)
          (format-time-string org-download-timestamp)
          (or ext (file-name-extension filename))))

;;;###autoload
(defun +org-attach*insert-link (_link filename)
  "TODO"
  (if (looking-back "^[ \t]+" (line-beginning-position))
      (delete-region (match-beginning 0) (match-end 0))
    (newline))
  (cond ((image-type-from-file-name filename)
         (when (file-in-directory-p filename org-attach-directory)
           (setq filename (file-relative-name filename +org-dir)))
         (insert
          (concat (if (= org-download-image-html-width 0)
                      ""
                    (format "#+attr_html: :width %dpx\n" org-download-image-html-width))
                  (if (= org-download-image-latex-width 0)
                      ""
                    (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width))
                  (format org-download-link-format
                          (file-relative-name filename (file-name-directory buffer-file-name)))))
         (org-display-inline-images))
        (t
         (insert
          (format "%s [[./%s][%s]] "
                  (+org-attach--icon filename)
                  (file-relative-name filename (file-name-directory buffer-file-name))
                  (file-name-nondirectory (directory-file-name filename)))))))

;;;###autoload
(defun +org-attach*relative-to-attach-dir (orig-fn &rest args)
  "TODO"
  (if (file-in-directory-p buffer-file-name +org-dir)
      (let* ((context (save-match-data (org-element-context)))
             (file (org-link-unescape (org-element-property :path context)))
             (default-directory
               (if (file-in-directory-p file org-attach-directory)
                   +org-dir
                 default-directory)))
        (apply orig-fn args))
    (apply orig-fn args)))
