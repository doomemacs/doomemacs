;;; feature/jump/autoload.el

;;;###autoload
(defun +jump/definition (&optional other-window)
  "Find definition, falling back to dumb-jump, then
`evil-goto-definition' otherwise."
  (interactive "p")
  (let ((orig-pt (point))
        (orig-file (buffer-file-name))
        (sym (thing-at-point 'symbol t)))
    (cond ((ignore-errors (xref-find-definitions sym)
                          t))

          ((and (fboundp 'dumb-jump-go)
                (progn (dumb-jump-go)
                       (and (= orig-pt (point))
                            (equal (file-truename orig-file)
                                   (file-truename buffer-file-name))))))

          ((fboundp 'counsel-ag)
           (counsel-ag sym (doom-project-root)))

          (t (error "Couldn't find '%s'" sym)))))

;;;###autoload
(defun +jump/references (&optional other-window)
  "TODO"
  (interactive "p")
  (let ((sym (thing-at-point 'symbol t)))
    (cond ((progn
             (ignore-errors (xref-find-references sym)
                            t)))

          ((fboundp 'counsel-ag)
           (counsel-ag sym (doom-project-root)))

          (t (error "Couldn't find '%s'" sym)))))

;;;###autoload
(defun +jump/online (where &optional search)
  "TODO"
  (interactive
   (list (completing-read "Search on: "
                          (mapcar #'car +lookup-search-url-alist)
                          nil t)))
  (let ((url (cdr (assoc where +lookup-search-url-alist)))
        (search (or search (read-string "Query: "))))
    (browse-url (format url (url-encode-url search)))))

