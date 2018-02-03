;;; org/org/autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode +org-pretty-mode
  "TODO"
  :init-value nil
  :lighter " *"
  :group 'evil-org
  (setq org-hide-emphasis-markers +org-pretty-mode)
  (org-toggle-pretty-entities)
  (org-with-silent-modifications
   ;; In case the above un-align tables
   (org-table-map-tables 'org-table-align t)))

;;;###autoload
(defun +org|realign-table-maybe ()
  "Auto-align table under cursor and re-calculate formulas."
  (when (org-at-table-p)
    (save-excursion
      (quiet! (org-table-recalculate)))))

;;;###autoload
(defun +org|update-cookies ()
  "Update counts in headlines (aka \"cookies\")."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (org-update-statistics-cookies t)))

;;;###autoload
(defun +org/dwim-at-point ()
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote definition: jump to the footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive)
  (let* ((scroll-pt (window-start))
         (context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (pcase type
      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (`headline
       (cond ((org-element-property :todo-type context)
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done) 'todo 'done)))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             (t
              (org-remove-latex-fragment-image-overlays)
              (org-toggle-latex-fragment '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-definition
       (goto-char (org-element-property :post-affiliated context))
       (call-interactively #'org-footnote-action))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
           (org-table-calc-current-TBLFM)
         (ignore-errors
           (save-excursion
             (goto-char (org-element-property :contents-begin context))
             (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
                  (bound-and-true-p evil-mode))
         (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies nil)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block))

      ((or `latex-fragment `latex-environment)
       (org-toggle-latex-fragment))

      (`link
       (let ((path (org-element-property :path (org-element-lineage context '(link) t))))
         (if (and path (image-type-from-file-name path))
             (+org/refresh-inline-images)
           (org-open-at-point))))

      (_ (+org/refresh-inline-images)))
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
  (let* ((context
          (save-excursion
            (when (bolp)
              (back-to-indentation)
              (forward-char))
            (org-element-lineage
             (org-element-context)
             '(table table-row headline inlinetask item plain-list)
             t)))
         (type (org-element-type context)))
    (cond ((memq type '(item plain-list))
           (let ((marker (org-element-property :bullet context))
                 (pad (save-excursion
                        (back-to-indentation)
                        (- (point) (line-beginning-position))))
                 afterp)
             (save-match-data
               (pcase direction
                 ('below
                  (org-end-of-item)
                  (backward-char)
                  (end-of-line)
                  (if (and marker (string-match "\\([0-9]+\\)\\([).] *\\)" marker))
                      (let ((l (line-number-at-pos)))
                        (org-insert-item)
                        (when (= l (line-number-at-pos))
                          (org-next-item)
                          (org-end-of-line)))
                    (insert "\n" (make-string pad 32) (or marker ""))))
                 ('above
                  (goto-char (line-beginning-position))
                  (if (and marker (string-match-p "[0-9]+[).]" marker))
                      (org-insert-item)
                    (insert (make-string pad 32) (or marker ""))
                    (save-excursion (insert "\n")))))))
           (when (org-element-property :checkbox context)
             (insert "[ ] ")))

          ((memq type '(table table-row))
           (pcase direction
             ('below (save-excursion (org-table-insert-row t))
                     (org-table-next-row))
             ('above (save-excursion (org-shiftmetadown))
                     (+org/table-previous-row))))

          ((memq type '(headline inlinetask))
           (let ((level (if (eq (org-element-type context) 'headline)
                            (org-element-property :level context)
                          1)))
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
                  (save-excursion (insert "\n")))))
             (when (org-element-property :todo-type context)
               (org-todo 'todo))))

          (t (user-error "Not a valid list, heading or table")))

    (when (org-invisible-p)
      (org-show-subtree))
    (when (bound-and-true-p evil-mode)
      (evil-insert 1))))

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
with `org-cycle'). Also:

  + If in a babel block, removes result blocks.
  + If in a table, realign it, if necessary."
  (interactive)
  (save-excursion
    (org-beginning-of-line)
    (cond ((org-at-table-p)
           (org-table-align))
          ((org-in-src-block-p)
           (org-babel-remove-result))
          ((org-at-heading-p)
           (outline-toggle-children))
          ((org-at-item-p)
           (let ((window-beg (window-start)))
             (org-cycle)
             (set-window-start nil window-beg))))))

;;;###autoload
(defun +org/remove-link ()
  "Unlink the text at point."
  (interactive)
  (unless (org-in-regexp org-bracket-link-regexp 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((remove (list (match-beginning 0) (match-end 0)))
          (description (if (match-end 3)
                           (match-string-no-properties 3)
                         (match-string-no-properties 1))))
      (apply #'delete-region remove)
      (insert description))))

