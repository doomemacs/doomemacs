;;; defuns-org-attach.el --- custom attachment system

;;;###autoload (autoload 'narf:org-attach "defuns-org-attach" nil t)
(evil-define-command narf:org-attach (&optional bang link)
  (interactive "<!><a>")
  (if (not link)
      (narf/org-attachment-list)
    (require 'org-download)
    (let ((new-path (if bang
                        (format "%s/%s" (expand-file-name org-download-image-dir org-directory)
                                (format "%s%s.%s"
                                        (f-base link)
                                        (format-time-string org-download-timestamp)
                                        (file-name-extension link))) buffer-file-name
                                        (org-download--fullname link))))
      (when new-path
        (cond ((string-match-p "^https?://" link)
               (url-copy-file link new-path))
              (t (copy-file link new-path)))
        (insert (format "[[./%s]]" (f-relative new-path default-directory)))))))

;; TODO Improve
(defun narf/org-reveal-attachments ()
  (interactive)
  (let ((context (org-element-context)))
    (narf-open-with
     nil
     (if (and context (eq (org-element-type context) 'link))
         (f-dirname (org-element-property :path context))
       org-download-image-dir))))

;;;###autoload
(defun narf--org-attachments ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (and (string= (org-element-property :type link) "file")
                 (string-prefix-p (format "./%s" org-download-image-dir)
                                  (org-element-property :path link)))
        (org-element-property :path link)))))

;; (defun narf--org-attachments-in-dir ()
;;   (-map (lambda (f) (concat "./" (f-relative f)))
;;         (append (f-entries org-download-image-dir)
;;                 (unless (f-same?
;;                          (expand-file-name org-download-image-dir)
;;                          (expand-file-name org-download-image-dir org-directory))
;;                   (f-entries (expand-file-name org-download-image-dir org-directory)))))
;;   )

(defun narf--org-attachment-real-to-display (real)
  (format "[%s] %s"
          (if (file-exists-p real) "X" "")
          (f-filename real)))

;; TODO Add delete action
;; TODO Goto link on select
(defun narf-org-attachment-source ()
  (helm-build-sync-source "Attachments"
    :candidates (narf--org-attachments)
    :real-to-display 'narf--org-attachment-real-to-display
    :action (lambda (f) (narf-open-with nil (f-dirname f)))))

;; TODO Organize this better
;;;###autoload
(defun narf/org-attachment-list ()
  (interactive)
  (helm :sources (narf-org-attachment-source)))

;; TODO
;; (defun narf/org-attachment-cleanup (&optional file)
;;   (interactive)
;;   )

;; TODO
;; (defun narf/org-attachment-cleanup-all ()
;;   (interactive)
;;   (dolist (file org-agenda-files)
;;     (narf/org-attachment-cleanup file)))

(provide 'defuns-org-attach)
;;; defuns-org-attach.el ends here
