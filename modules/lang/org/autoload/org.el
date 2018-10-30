;;; org/org/autoload/org.el -*- lexical-binding: t; -*-

(defun +org--get-property (name &optional bound)
  (save-excursion
    (let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
      (goto-char (point-min))
      (when (re-search-forward re bound t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

;;;###autoload
(defun +org-get-property (name &optional file bound)
  "Get a document property named NAME (string) from an org FILE (defaults to
current file). Only scans first 2048 bytes of the document."
  (unless bound
    (setq bound 2048))
  (if file
      (with-temp-buffer
        (insert-file-contents-literally file nil 0 bound)
        (+org--get-property name))
    (+org--get-property name bound)))

;;;###autoload
(defun +org-get-todo-keywords-for (keyword)
  "TODO"
  (when keyword
    (cl-loop for (type . keyword-spec) in org-todo-keywords
             for keywords = (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                               (match-string 1 x)
                                             x))
                                    keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))


;;
;; Modes

;;;###autoload
(define-minor-mode +org-pretty-mode
  "Hides emphasis markers and toggles pretty entities."
  :init-value nil
  :lighter " *"
  :group 'evil-org
  (setq org-hide-emphasis-markers +org-pretty-mode)
  (org-toggle-pretty-entities)
  (org-with-silent-modifications
   ;; In case the above un-align tables
   (org-table-map-tables 'org-table-align t)))


;;
;; Commands

;;;###autoload
(defun +org/dwim-at-point ()
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive)
  (let* ((context (org-element-context))
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
               (if (eq (org-element-property :todo-type context) 'done)
                   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                       'todo)
                 'done)))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             (t
              (+org/refresh-inline-images)
              (org-remove-latex-fragment-image-overlays)
              (org-toggle-latex-fragment '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-reference
       (org-footnote-goto-definition (org-element-property :label context)))

      (`footnote-definition
       (org-footnote-goto-previous-reference (org-element-property :label context)))

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
       (let* ((lineage (org-element-lineage context '(link) t))
              (path (org-element-property :path lineage)))
         (if (or (equal (org-element-property :type lineage) "img")
                 (and path (image-type-from-file-name path)))
             (+org/refresh-inline-images)
           (org-open-at-point))))

      (_ (+org/refresh-inline-images)))))

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
                        (org-beginning-of-item)
                        (back-to-indentation)
                        (- (point) (line-beginning-position)))))
             (save-match-data
               (pcase direction
                 (`below
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
                 (`above
                  (org-beginning-of-item)
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
               (`below
                (let ((at-eol (>= (point) (1- (line-end-position))))
                      org-insert-heading-respect-content)
                  (goto-char (line-end-position))
                  (org-end-of-subtree)
                  (insert (concat "\n"
                                  (when (= level 1)
                                    (if at-eol
                                        (ignore (cl-incf level))
                                      "\n"))
                                  (make-string level ?*)
                                  " "))))
               (`above
                (org-back-to-heading)
                (insert (make-string level ?*) " ")
                (save-excursion
                  (insert "\n")
                  (if (= level 1) (insert "\n")))))
             (when-let* ((todo-keyword (org-element-property :todo-keyword context)))
               (org-todo (or (car (+org-get-todo-keywords-for todo-keyword))
                             'todo)))))

          (t (user-error "Not a valid list, heading or table")))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (bound-and-true-p evil-mode)
      (evil-insert 1))))

;;;###autoload
(defun +org/dedent ()
  "TODO"
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
        ((call-interactively #'self-insert-command))))

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

;;;###autoload
(defun +org/toggle-checkbox ()
  "Toggle the presence of a checkbox in the current item."
  (interactive)
  (org-toggle-checkbox '(4)))

;;;###autoload
(defalias #'+org/toggle-fold #'+org|cycle-only-current-subtree)

;;;###autoload
(defun +org/open-fold ()
  "Open the current fold (not but its children)."
  (interactive)
  (+org/toggle-fold t))

;;;###autoload
(defalias #'+org/close-fold #'outline-hide-subtree)

(defun +org--get-foldlevel ()
  (let ((max 1))
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (org-next-visible-heading 1)
          (when (outline-invisible-p (line-end-position))
            (let ((level (org-outline-level)))
              (when (> level max)
                (setq max level))))))
      max)))

;;;###autoload
(defun +org/show-next-fold-level ()
  "Decrease the fold-level of the visible area of the buffer. This unfolds
another level of headings on each invocation."
  (interactive)
  (let* ((current-level (+org--get-foldlevel))
         (new-level (1+ current-level)))
    (outline-hide-sublevels new-level)
    (message "Folded to level %s" new-level)))

;;;###autoload
(defun +org/hide-next-fold-level ()
  "Increase the global fold-level of the visible area of the buffer. This folds
another level of headings on each invocation."
  (interactive)
  (let* ((current-level (+org--get-foldlevel))
         (new-level (max 1 (1- current-level))))
    (outline-hide-sublevels new-level)
    (message "Folded to level %s" new-level)))


;;
;; Hooks

;;;###autoload
(defun +org|delete-backward-char-and-realign-table-maybe ()
  "TODO"
  (when (eq major-mode 'org-mode)
    (org-check-before-invisible-edit 'delete-backward)
    (save-match-data
      (when (and (org-at-table-p)
                 (not (org-region-active-p))
                 (string-match-p "|" (buffer-substring (point-at-bol) (point)))
                 (looking-at-p ".*?|"))
        (let ((pos (point))
              (noalign (looking-at-p "[^|\n\r]*  |"))
              (c org-table-may-need-update))
          (delete-char -1)
          (unless overwrite-mode
            (skip-chars-forward "^|")
            (insert " ")
            (goto-char (1- pos)))
          ;; noalign: if there were two spaces at the end, this field
          ;; does not determine the width of the column.
          (when noalign (setq org-table-may-need-update c)))
        t))))

;;;###autoload
(defun +org|indent-maybe ()
  "Indent the current item (header or item), if possible. Made for
`org-tab-first-hook' in evil-mode."
  (interactive)
  (cond ((or (not (bound-and-true-p evil-mode))
             (not (eq evil-state 'insert)))
         nil)
        ((org-at-item-p)
         (if (eq this-command 'org-shifttab)
             (org-outdent-item-tree)
           (org-indent-item-tree))
         t)
        ((org-at-heading-p)
         (ignore-errors
           (if (eq this-command 'org-shifttab)
               (org-promote)
             (org-demote)))
         t)
        ((org-in-src-block-p t)
         (org-babel-do-in-edit-buffer
          (call-interactively #'indent-for-tab-command))
         t)))

;;;###autoload
(defun +org|realign-table-maybe ()
  "Auto-align table under cursor and re-calculate formulas."
  (when (and (org-at-table-p) org-table-may-need-update)
    (let ((pt (point))
          (inhibit-message t))
      (org-table-recalculate)
      (if org-table-may-need-update (org-table-align))
      (goto-char pt))))

;;;###autoload
(defun +org|update-cookies ()
  "Update counts in headlines (aka \"cookies\")."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let (org-hierarchical-todo-statistics)
      (org-update-parent-todo-statistics))))

;;;###autoload
(defun +org|yas-expand-maybe ()
  "Tries to expand a yasnippet snippet, if one is available. Made for
`org-tab-first-hook'."
  (when (and (or (not (bound-and-true-p evil-mode))
                 (eq evil-state 'insert))
             (bound-and-true-p yas-minor-mode)
             (yas--templates-for-key-at-point))
    (call-interactively #'yas-expand)
    t))

;;;###autoload
(defun +org|cycle-only-current-subtree (&optional arg)
  "Toggle the local fold at the point (as opposed to cycling through all levels
with `org-cycle')."
  (interactive "P")
  (unless (eq this-command 'org-shifttab)
    (save-excursion
      (org-beginning-of-line)
      (let (invisible-p)
        (when (and (org-at-heading-p)
                   (or org-cycle-open-archived-trees
                       (not (member org-archive-tag (org-get-tags))))
                   (or (not arg)
                       (setq invisible-p (outline-invisible-p (line-end-position)))))
          (unless invisible-p
            (setq org-cycle-subtree-status 'subtree))
          (org-cycle-internal-local)
          t)))))

;;;###autoload
(defun +org|remove-occur-highlights ()
  "Remove org occur highlights on ESC in normal mode."
  (when org-occur-highlights
    (org-remove-occur-highlights)
    t))


;;
;; Advice

;;;###autoload
(defun +org*fix-newline-and-indent-in-src-blocks ()
  "Try to mimic `newline-and-indent' with correct indentation in src blocks."
  (when (org-in-src-block-p t)
    (org-babel-do-in-edit-buffer
     (call-interactively #'indent-for-tab-command))))

;;;###autoload
(defun +org*realign-table-maybe (&rest _)
  "Auto-align table under cursor and re-calculate formulas."
  (when (eq major-mode 'org-mode)
    (+org|realign-table-maybe)))

;;;###autoload
(defun +org*evil-org-open-below (orig-fn count)
  "Fix o/O creating new list items in the middle of nested plain lists. Only has
an effect when `evil-org-special-o/O' has `item' in it (not the default)."
  (cl-letf (((symbol-function 'end-of-visible-line)
             (lambda ()
               (org-end-of-item)
               (backward-char 1)
               (evil-append nil))))
    (funcall orig-fn count)))
