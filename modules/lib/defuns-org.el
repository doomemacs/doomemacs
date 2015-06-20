;;; defuns-org.el

;;;###autoload
(defun narf/org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change. Hides properties permanently."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max)
                    (if (eq state 'children)
                        (save-excursion (outline-next-heading) (point))
                      (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (backward-char 1)
            (let ((b (point)))
              (if (re-search-forward
                   "^[ \t]*:END:"
                   (save-excursion (outline-next-heading) (point)) t)
                  (outline-flag-region b (point-at-eol) t)
                (user-error ":END: line missing at position %s" b)))))))))

(defun narf--org-in-list-p ()
  (and (save-excursion (search-backward-regexp "^ *\\([0-9]+[\.)]\\|[-*+]\\) "
                                               (line-beginning-position) t))
       (org-in-item-p)))

;;;###autoload
(defun narf/project-org-filename (cat)
  (interactive (list (completing-read "Choose category:"
                                      (mapcar 'f-filename (f-directories org-project-directory)))))
  (expand-file-name (concat (file-name-nondirectory (directory-file-name (narf/project-root))) ".org")
                    (expand-file-name cat org-project-directory)))

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

;;;###autoload (autoload 'narf:org-insert-image-url "defuns-org" nil t)
(evil-define-command narf:org-insert-image-url (&optional image-url)
  :repeat nil
  (interactive "<f>")
  (unless image-url
    (user-error "You must specify an image URL to insert"))
  (let ((dest (f-join org-directory "images/" (concat (format-time-string "%Y%m%d-") (f-filename image-url)))))
    (shell-command (format "wget '%s' -O '%s'" image-url dest))
    (insert (format "<%s>" (f-relative dest (f-dirname (buffer-file-name)))))
    (indent-according-to-mode)))

;;;###autoload (autoload 'narf:org-insert-image "defuns-org" nil t)
(evil-define-command narf:org-insert-image (&optional filename bang)
  :repeat nil
  (interactive "<f><!>")
  (if bang
      (narf:org-insert-image-url filename)
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

;;;###autoload (autoload 'narf:org-search-files-or-headers "defuns-org" nil t)
(evil-define-command narf:org-search-files-or-headers (&optional bang)
  (interactive "<!>")
  (require 'org)
  (if bang
      (ido-find-file-in-dir org-directory)
    (call-interactively 'helm-org-agenda-files-headings)))

(provide 'defuns-org)
;;; defuns-org.el ends here
