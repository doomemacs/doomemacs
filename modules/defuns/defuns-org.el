;;; defuns-org.el

;;;###autoload
(defun doom/org-indent ()
  "Indent the current item (header or item). Otherwise, forward to
`self-insert-command'."
  (interactive)
  (cond ((org-at-item-p)
         (org-indent-item-tree))
        ((and (org-on-heading-p)
              (looking-back "^\\*+ +.*" (line-beginning-position)))
         (ignore-errors (org-demote)))
        (t (call-interactively 'self-insert-command))))

;;;###autoload
(defun doom/org-indent-or-next-field ()
  "Depending on the context either indent the current item or go the next table field."
  (interactive)
  (call-interactively (if (org-at-table-p) 'org-table-next-field 'doom/org-indent)))

;;;###autoload
(defun doom/org-dedent ()
  "Dedent the current item (header or item). Otherwise, forward to
`self-insert-command'."
  (interactive)
  (cond ((org-at-item-p)
         (let ((struct (if (org-region-active-p)
                           (save-excursion (goto-char (region-beginning))
                                           (org-list-struct))
                         (org-list-struct))))
           (org-list-indent-item-generic -1 nil struct)))
        ((and (org-on-heading-p)
              (looking-back "^\\*+ +.*" (line-beginning-position)))
         (ignore-errors
           (org-promote)))
        (t (call-interactively 'self-insert-command))))

;;;###autoload
(defun doom/org-dedent-or-prev-field ()
  "Depending on the context either dedent the current item or go the previous
table field."
  (interactive)
  (call-interactively (if (org-at-table-p) 'org-table-previous-field 'doom/org-dedent)))

;;;###autoload
(defun doom/org-insert-item (direction)
  "Inserts a new heading, table cell or item, depending on the context.
DIRECTION can be 'above or 'below.

I use this instead of `org-insert-item' or `org-insert-heading' which are too
opinionated and perform this simple task incorrectly (e.g. whitespace in the
wrong places)."
  (interactive)
  (let* ((context (org-element-lineage
                   (org-element-context)
                   '(table table-row headline inlinetask item plain-list)
                   t))
         (type (org-element-type context)))
    (cond ((eq type 'item)
           (let ((marker (org-element-property :bullet context)))
             (cl-case direction
               ('below
                (goto-char (line-end-position))
                (insert (concat "\n" marker)))
               ('above
                (goto-char (line-beginning-position))
                (insert marker)
                (save-excursion (insert "\n")))))
           (when (org-element-property :checkbox context)
             (insert "[ ] ")))
          ((memq type '(table table-row))
           (cl-case direction
             ('below (org-table-insert-row t))
             ('above (doom/org-table-prepend-row-or-shift-up))))
          (t
           (let ((level (save-excursion
                          (org-back-to-heading)
                          (org-element-property
                           :level (org-element-lineage (org-element-context)
                                                       '(headline) t)))))
             (cl-case direction
               ('below
                (let ((at-eol (= (point) (1- (line-end-position)))))
                  (goto-char (line-end-position))
                  (org-end-of-subtree)
                  (insert (concat "\n"
                                  (when (= level 1)
                                    (if at-eol
                                        (ignore (cl-incf level))
                                      "\n"))
                                  (make-string level ?*)
                                  " "))))
               ('above
                (org-back-to-heading)
                (org-insert-heading)
                (when (= level 1)
                  (save-excursion (evil-open-above 1))
                  (save-excursion (insert "\n")))))
             (when (org-element-property :todo-type context)
               (org-todo 'todo)))))
    (evil-append-line 1)))

;;;###autoload
(defun doom/org-toggle-checkbox ()
  (interactive)
  (let ((context (org-element-lineage (org-element-context) '(item) t)))
    (when context
      (org-end-of-line)
      (org-beginning-of-line)
      (if (org-element-property :checkbox context)
          (when (search-backward-regexp "\\[[ +-]\\]" (line-beginning-position) t)
            (delete-char 4))
        (insert "[ ] ")))))

;;;###autoload
(defun doom/org-toggle-fold ()
  "Toggle the local fold at the point (as opposed to cycling through all levels
with `org-cycle')."
  (interactive)
  (cond ((org-at-heading-p)
         (outline-toggle-children))
        ((org-at-item-p)
         (org-cycle))))

;;;###autoload
(defun doom/org-dwim-at-point ()
  "Do-what-I-mean at point. This includes following timestamp links, aligning
tables, toggling checkboxes/todos, executing babel blocks, previewing latex
fragments, opening links, or refreshing images."
  (interactive)
  (let* ((scroll-pt (window-start))
         (context (org-element-context))
         (type (org-element-type context))
         (value (org-element-property :value context)))
    (cond
     ((memq type '(planning timestamp))
      (org-follow-timestamp-link))

     ((memq type '(table table-row))
      (if (org-element-property :tblfm (org-element-property :parent context))
          (org-table-recalculate t)
        (org-table-align)))

     ((and (memq type '(item))
           (org-element-property :checkbox context))
      (org-toggle-checkbox))

     ((and (memq type '(headline))
           (org-element-property :todo-type context))
      (org-todo
       (if (eq (org-element-property :todo-type context) 'done) 'todo 'done)))

     ((and (memq type '(headline))
           (string= "ARCHIVE" (car-safe (org-get-tags))))
      (org-force-cycle-archived))

     ((memq type '(headline))
      (org-remove-latex-fragment-image-overlays)
      (org-preview-latex-fragment '(4)))

     ((memq type '(babel-call))
      (org-babel-lob-execute-maybe))

     ((memq type '(src-block inline-src-block))
      (org-babel-execute-src-block))

     ((memq type '(latex-fragment latex-environment))
      (org-preview-latex-fragment))

     ((memq type '(link))
      (let ((path (org-element-property :path (org-element-lineage (org-element-context) '(link) t))))
        (if (and path (image-type-from-file-name path))
            (doom/org-refresh-inline-images)
          (org-open-at-point))))

     (t (doom/org-refresh-inline-images)))
    (set-window-start nil scroll-pt)))

;;;###autoload
(defun doom/org-refresh-inline-images ()
  "Refresh image previews in the current heading/tree."
  (interactive)
  (if (> (length org-inline-image-overlays) 0)
      (org-remove-inline-images)
    (org-display-inline-images
     t t
     (if (org-before-first-heading-p)
         (line-beginning-position)
       (save-excursion (org-back-to-heading) (point)))
     (if (org-before-first-heading-p)
         (line-end-position)
       (save-excursion (org-end-of-subtree) (point))))))

;;;###autoload
(defun doom/org-surround (delim)
  "Surround the cursor (or selected region) with DELIM."
  (if (region-active-p)
      (save-excursion
        (goto-char (region-beginning))
        (insert delim)
        (goto-char (region-end))
        (insert delim))
    (insert delim)
    (save-excursion (insert delim))))


;;
;; tables
;;

;;;###autoload
(defun doom/org-table-next-row ()
  (interactive)
  (if (org-at-table-p) (org-table-next-row) (org-down-element)))

;;;###autoload
(defun doom/org-table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (if (org-at-table-p)
      (progn
        (org-table-maybe-eval-formula)
        (org-table-maybe-recalculate-line)
        (if (and org-table-automatic-realign
                 org-table-may-need-update)
            (org-table-align))
        (let ((col (org-table-current-column)))
          (beginning-of-line 0)
          (when (or (not (org-at-table-p)) (org-at-table-hline-p))
            (beginning-of-line))
          (org-table-goto-column col)
          (skip-chars-backward "^|\n\r")
          (when (org-looking-at-p " ") (forward-char))))
    (org-up-element)))

;;;###autoload
(defun doom/org-table-next-field ()
  (interactive)
  (if (org-at-table-p) (org-table-next-field) (org-end-of-line)))

;;;###autoload
(defun doom/org-table-previous-field ()
  (interactive)
  (if (org-at-table-p) (org-table-previous-field) (org-beginning-of-line)))

;;;###autoload
(defun doom/org-table-append-field-or-shift-right ()
  (interactive)
  (org-shiftmetaright)
  (when (org-at-table-p) (org-metaright)))

;;;###autoload
(defun doom/org-table-prepend-field-or-shift-left ()
  (interactive)
  (if (org-at-table-p) (org-shiftmetaright) (org-shiftmetaleft)))

;;;###autoload
(defun doom/org-table-append-row-or-shift-down ()
  (interactive)
  (org-shiftmetadown)
  (when (org-at-table-p) (org-metadown)))

;;;###autoload
(defun doom/org-table-prepend-row-or-shift-up ()
  (interactive)
  (if (org-at-table-p)
      (org-shiftmetadown)
    (org-shiftmetaup)))


;;
;; Links
;;

;;;###autoload (autoload 'doom:org-link "defuns-org" nil t)
(evil-define-command doom:org-link (link)
  "Add LINK to the org buffer. If a selection is active, link selection to LINK."
  (interactive "<a>")
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let ((beg evil-visual-beginning)
        (end evil-visual-end))
    (org-insert-link nil link (when (and beg end) (buffer-substring-no-properties beg end)))))

;;;###autoload
(defun doom/org-remove-link ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
            (description (if (match-end 3)
                             (org-match-string-no-properties 3)
                           (org-match-string-no-properties 1))))
        (apply 'delete-region remove)
        (insert description))))


;;
;; org-capture
;;

;;;###autoload
(defun doom/org-capture (&optional template string)
  "Run `org-capture' in a new, disposable popup frame."
  (interactive)
  (let ((org-capture-entry (org-capture-select-template template)))
    (cond ((equal org-capture-entry "C")
           (find-file (expand-file-name "module-org-notes.el" doom-modules-dir))
           (re-search-forward "^\\s-+(setq org-capture-templates" (point-max) t)
           (recenter))
          ((not (equal org-capture-entry "q"))
           (let ((frame (make-frame '((name . "org-capture") (height . 15) (width . 80)))))
             (with-selected-frame frame
               (if string
                   (org-capture-string string)
                 (org-capture))))))))

;;;###autoload (autoload 'doom:org-capture "defuns-org" nil t)
(evil-define-operator doom:org-capture (&optional beg end bang)
  "Send a selection to `doom/org-capture'."
  :move-point nil
  :type inclusive
  (interactive "<r><!>")
  (doom/org-capture
   (if (and (evil-visual-state-p) beg end)
       (buffer-substring beg end)
     "")))


;;
;; attachments
;;

;;;###autoload (autoload 'doom:org-attach "defuns-org-notes" nil t)
(evil-define-command doom:org-attach (&optional uri)
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
                                     " " (doom--org-attach-icon rel-path)))

          (insert (if image-p
                      (format "[[./%s]] " rel-path)
                    (format "%s [[./%s][%s]] "
                            (doom--org-attach-icon rel-path)
                            rel-path (f-filename rel-path)))))
        (when (string-match-p (regexp-opt '("jpg" "jpeg" "gif" "png")) (f-ext rel-path))
          (org-redisplay-inline-images)))
    (let ((default-directory ".attach/"))
      (if (file-exists-p default-directory)
          (call-interactively 'find-file)
        (user-error "No attachments")))))

(defun doom--org-attach-icon (path)
  (char-to-string (pcase (downcase (f-ext path))
                    ("jpg" ?) ("jpeg" ?) ("png" ?) ("gif" ?)
                    ("pdf" ?)
                    ("ppt" ?) ("pptx" ?)
                    ("xls" ?) ("xlsx" ?)
                    ("doc" ?) ("docx" ?)
                    ("ogg" ?) ("mp3" ?) ("wav" ?)
                    ("mp4" ?) ("mov" ?) ("avi" ?)
                    ("zip" ?) ("gz" ?) ("tar" ?) ("7z" ?) ("rar" ?)
                    (_ ?))))

;;;###autoload
(defun doom/org-cleanup-attachments ()
  ;; "Deletes any attachments that are no longer present in the org-mode buffer."
  (let* ((attachments-local (doom-org-attachments))
         (attachments (f-entries org-attach-directory))
         (to-delete (-difference attachments-local attachments)))
    ;; TODO
    to-delete))

(defun doom-org-attachments ()
  (unless (eq major-mode 'org-mode)
    (user-error "Not an org buffer"))
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
(defun doom/org-download-dnd (uri action)
  (if (eq major-mode 'org-mode)
      (doom:org-attach uri)
    (let ((dnd-protocol-alist
           (rassq-delete-all 'doom/org-download-dnd (copy-alist dnd-protocol-alist))))
      (dnd-handle-one-url nil action uri))))

(provide 'defuns-org)
;;; defuns-org.el ends here
