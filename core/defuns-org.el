;;;###autoload
(defun narf/project-org-filename (cat)
  (interactive (list (completing-read "Choose category:"
                                      (mapcar 'f-filename (f-directories org-project-directory)))))
  (expand-file-name (concat (file-name-nondirectory (directory-file-name (narf/project-root))) ".org")
                    (expand-file-name cat org-project-directory)))

;;;###autoload
(defun narf--org-in-list-p ()
  (and (save-excursion (search-backward-regexp "^ *\\([0-9]+[\.)]\\|[-*+]\\) "
                                               (line-beginning-position) t))
       (org-in-item-p)))

;;;###autoload
(defun narf/org-insert-item-after ()
  "Inserts a new heading or item, depending on the context."
  (interactive)
  (org-end-of-line)
  (cond ((org-at-item-checkbox-p)
         (org-insert-heading)
         (insert "[ ] "))
        ((narf--org-in-list-p)
         (org-insert-heading))
        ((org-on-heading-p)
         (org-insert-heading-after-current))
        (t
         (org-insert-heading-after-current)
         (delete-char 1)))
  (evil-insert-state))

;; TODO Check if this and -forward can be combined
;;;###autoload
(defun narf/org-insert-item-before ()
  "Inserts a new heading or item, depending on the context."
  (interactive)
  (evil-first-non-blank)
  (cond ((org-at-item-checkbox-p)
         (org-insert-heading)
         (insert "[ ] "))
        ((narf--org-in-list-p)
         (org-insert-heading))
        (t (org-insert-heading)))
  (evil-insert-state))

;;;###autoload
(defun narf/org-toggle-checkbox ()
  (interactive)
  (save-excursion
    (org-end-of-line)
    (cond ((org-in-item-p)
           (if (search-backward-regexp "\\[[ +-]\\]" (line-beginning-position) t)
               (delete-char 4)
             (org-beginning-of-line)))
          (t (org-insert-heading)))
    (insert "[ ] ")))

;; Formatting shortcuts
;;;###autoload
(defun narf/org-surround (delim)
  (insert delim) (save-excursion (insert delim)))

;;;###autoload (autoload 'narf::org-insert-image-url "defuns-org")
(evil-define-command narf::org-insert-image-url (&optional image-url)
  :repeat nil
  (interactive "<f>")
  (unless image-url
    (user-error "You must specify an image URL to insert"))
  (let ((dest (f-join org-directory "images/" (concat (format-time-string "%Y%m%d-") (f-filename image-url)))))
    (shell-command (format "wget '%s' -O '%s'" image-url dest))
    (insert (format "<%s>" (f-relative dest (f-dirname (buffer-file-name)))))
    (indent-according-to-mode)))

;;;###autoload (autoload 'narf::org-insert-image "defuns-org")
(evil-define-command narf::org-insert-image (&optional filename bang)
  :repeat nil
  (interactive "<f><!>")
  (if bang
      (narf::org-insert-image-url filename)
    (unless filename
      (user-error "You must specify a file to attach"))
    (unless (file-exists-p filename)
      (user-error "File %s does not exist" filename))
    (let ((dest (f-join org-directory "images/" (concat (format-time-string "%Y%m%d-") (f-filename filename)))))
      (when (file-exists-p dest)
        (user-error "File %s already exists at destination!"))
      (copy-file filename dest)
      (insert (format "<file:%s>" (f-relative dest (f-dirname (buffer-file-name)))))
      (indent-according-to-mode))))


(provide 'defuns-org)
;;; defuns-org.el ends here
