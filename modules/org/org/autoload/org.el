;;; org/org/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode +org-pretty-mode
  "TODO"
  :init-value nil
  :lighter " *"
  :group 'evil-org
  (setq org-hide-emphasis-markers +org-pretty-mode)
  (org-toggle-pretty-entities)
  ;; In case the above un-align tables
  (org-table-map-tables 'org-table-align t))

;;;###autoload
(defun +org|realign-table-maybe ()
  "Auto-align table under cursor."
  (when (org-at-table-p)
    (save-excursion
      (org-table-align))))

;;;###autoload
(defun +org|update-cookies ()
  "Update counts in headlines (aka \"cookies\")."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (org-update-statistics-cookies t)))

;;;###autoload
(defun +org/dwim-at-point ()
  "Do-what-I-mean at point. This includes following timestamp links, aligning
tables, toggling checkboxes/todos, executing babel blocks, previewing latex
fragments, opening links, or refreshing images."
  (interactive)
  (let* ((scroll-pt (window-start))
         (context (org-element-context))
         (type (org-element-type context)))
    (cond
     ((memq type '(planning timestamp))
      (org-follow-timestamp-link))

     ((memq type '(table table-row))
      (if (org-element-property :tblfm (org-element-property :parent context))
          (org-table-recalculate t)
        (org-table-align)))

     ((org-element-property :checkbox (org-element-lineage context '(item) t))
      (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
        (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

     ((and (eq type 'headline)
           (org-element-property :todo-type context))
      (org-todo
       (if (eq (org-element-property :todo-type context) 'done) 'todo 'done)))

     ((and (eq type 'headline)
           (string= "ARCHIVE" (car-safe (org-get-tags))))
      (org-force-cycle-archived))

     ((eq type 'headline)
      (org-remove-latex-fragment-image-overlays)
      (org-toggle-latex-fragment '(4)))

     ((eq type 'babel-call)
      (org-babel-lob-execute-maybe))

     ((memq type '(src-block inline-src-block))
      (org-babel-execute-src-block))

     ((memq type '(latex-fragment latex-environment))
      (org-toggle-latex-fragment))

     ((eq type 'link)
      (let ((path (org-element-property :path (org-element-lineage context '(link) t))))
        (if (and path (image-type-from-file-name path))
            (+org/refresh-inline-images)
          (org-open-at-point))))

     (t (+org/refresh-inline-images)))
    (set-window-start nil scroll-pt)))

;;;###autoload
(defun +org/indent ()
  "Indent the current item (header or item). Otherwise, forward to
`self-insert-command'."
  (interactive)
  (cond ((org-at-item-p)
         (org-indent-item-tree))
        ((org-at-heading-p)
         (ignore-errors (org-demote)))
        ((org-in-src-block-p t)
         (doom/dumb-indent))
        (t
         (call-interactively #'self-insert-command))))

;;;###autoload
(defun +org/indent-or-next-field-or-yas-expand ()
  "Depending on the context either a) indent the current line, b) go the next
table field or c) run `yas-expand'."
  (interactive)
  (call-interactively
   (cond ((and (bound-and-true-p yas-minor-mode)
               (yas--templates-for-key-at-point))
          #'yas-expand)
         ((org-at-table-p)
          #'org-table-next-field)
         (t
          #'+org/indent))))

;;;###autoload
(defun +org/dedent ()
  "Dedent the current item (header or item). Otherwise, forward to
`self-insert-command'."
  (interactive)
  (cond ((org-at-item-p)
         (org-list-indent-item-generic
          -1 nil
          (save-excursion
            (when (org-region-active-p)
              (goto-char (region-beginning)))
            (org-list-struct))))
        ((org-at-heading-p)
         (ignore-errors (org-promote)))
        (t
         (call-interactively #'self-insert-command))))

;;;###autoload
(defun +org/dedent-or-prev-field ()
  "Depending on the context either dedent the current item or go the previous
table field."
  (interactive)
  (call-interactively
   (if (org-at-table-p)
       #'org-table-previous-field
     #'+org/dedent)))

;;;###autoload
(defun +org/insert-item (direction)
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
           (let ((marker (org-element-property :bullet context))
                 (pad (save-excursion
                        (back-to-indentation)
                        (- (point) (line-beginning-position)))))
             (pcase direction
               ('below
                (goto-char (line-end-position))
                (insert (concat  "\n" (make-string pad ? ) marker)))
               ('above
                (goto-char (line-beginning-position))
                (insert (make-string pad ? ) marker)
                (save-excursion (insert "\n")))))
           (when (org-element-property :checkbox context)
             (insert "[ ] ")))

          ((memq type '(table table-row))
           (pcase direction
             ('below (org-table-insert-row t))
             ('above (+org/table-prepend-row-or-shift-up))))

          ((memq type '(headline inlinetask plain-list))
           (let* ((subcontext (org-element-context))
                  (level (save-excursion
                           (org-back-to-heading)
                           (org-element-property
                            :level
                            (if (eq (org-element-type subcontext) 'headline)
                                subcontext
                              1)))))
             (pcase direction
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
               (org-todo 'todo))))

          (t (user-error "Not a valid list, heading or table")))

    (when (bound-and-true-p evil-mode)
      (evil-append-line 1))))

;;;###autoload
(defun +org-get-property (name &optional _file) ; TODO Add FILE
  "Get a propery from an org file."
  (save-excursion
    (goto-char 1)
    (re-search-forward (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name)) nil t)
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

;;;###autoload
(defun +org/refresh-inline-images ()
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
(defun +org/toggle-checkbox ()
  "Toggle the presence of a checkbox in the current item."
  (interactive)
  (org-toggle-checkbox '(4)))

;;;###autoload
(defun +org/toggle-fold ()
  "Toggle the local fold at the point (as opposed to cycling through all levels
with `org-cycle'). Also removes babel result blocks, if run from a code block."
  (interactive)
  (save-excursion
    (org-beginning-of-line)
    (cond ((org-in-src-block-p)
           (org-babel-remove-result))
          ((org-at-heading-p)
           (outline-toggle-children))
          ((org-at-item-p)
           (let ((window-beg (window-start)))
             (org-cycle)
             (set-window-start nil window-beg))))))
