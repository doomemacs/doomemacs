;;; defuns-org-attach.el --- custom attachment system

;; I know Org has its own attachment system, but I don't like it.

;;;###autoload (autoload 'narf:org-attach "defuns-org-attach" nil t)
(evil-define-command narf:org-attach (&optional bang link)
  (interactive "<!><a>")
  (if (not link)
      (narf:org-attachment-list bang)
    (require 'org-download)
    (let ((new-path (if bang
                        (format "%s/%s" (expand-file-name org-download-image-dir org-directory)
                                (format "%s%s.%s"
                                        (f-base link)
                                        (format-time-string org-download-timestamp)
                                        (file-name-extension link))) buffer-file-name
                                        (org-download--fullname link))))
      (unless new-path
        (user-error "No file was provided"))
      (cond ((string-match-p "^https?://" link)
             (url-copy-file link new-path))
            (t (copy-file link new-path)))
      (insert (format "[[./%s]]" (f-relative new-path default-directory))))))

;;;###autoload (autoload 'narf:org-attachment-list "defuns-org-attach" nil t)
(evil-define-command narf:org-attachment-list (&optional bang)
  (interactive "<!>")
  (if bang
      (narf:org-attachment-cleanup)
    (let ((attachments (narf-org--get-attachments)))
      (unless attachments
        (user-error "No attachments in this file"))
      (helm :sources
            (helm-build-sync-source "Attachments"
              :candidates attachments
              :real-to-display 'narf-org--attachment-real-to-display
              :action '(("Go to Attachment in Buffer" . narf-org--attachment-find)
                        ("Reveal Attachment in Finder" . narf-org--attachment-reveal)
                        ("Open Attachment" . narf-org--attachment-open)
                        ("Delete Attachment" . narf-org--attachment-delete)))))))

;; TODO Improve
;;;###autoload
(defun narf/org-attachment-reveal ()
  (interactive)
  (let ((context (org-element-context)))
    (narf-open-with
     nil
     (if (and context (eq (org-element-type context) 'link))
         (f-dirname (org-element-property :path context))
       org-download-image-dir))))

(defun narf-org--get-attachments ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (and (string= (org-element-property :type link) "file")
                 (string-prefix-p (format "./%s" org-download-image-dir)
                                  (org-element-property :path link)))
        (org-element-property :path link)))))

(defun narf-org--attachment-real-to-display (real)
  (propertize (f-filename real) 'face (if (file-exists-p real) 'helm-ff-file 'shadow)))

(defun narf-org--attachment-find (file)
  (search-forward-regexp (format "[[\\(file:\\)?%s]]" file) nil t)
  (ignore-errors
      (save-excursion
        (outline-previous-visible-heading 1)
        (org-show-subtree))))

(defun narf-org--attachment-reveal (file)
  (narf-open-with nil (f-dirname file)))

(defun narf-org--attachment-open (file)
  (narf-open-with nil file))

(defun narf-org--attachment-delete (file)
  (delete-file file)
  (narf-org--attachment-find file)
  (message "File deleted, now delete the link! (%s)" file))

(provide 'defuns-org-attach)
;;; defuns-org-attach.el ends here
