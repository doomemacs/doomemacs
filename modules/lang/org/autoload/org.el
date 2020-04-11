;;; lang/org/autoload/org.el -*- lexical-binding: t; -*-

;;
;;; Helpers

(defun +org--refresh-inline-images-in-subtree ()
  "Refresh image previews in the current heading/tree."
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

(defun +org--insert-item (direction)
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

          ((let ((level (or (org-current-level) 1)))
             (pcase direction
               (`below
                (let (org-insert-heading-respect-content)
                  (goto-char (line-end-position))
                  (org-end-of-subtree)
                  (insert "\n" (make-string level ?*) " ")))
               (`above
                (org-back-to-heading)
                (insert (make-string level ?*) " ")
                (save-excursion (insert "\n"))))
             (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                         (todo-type (org-element-property :todo-type context)))
               (org-todo (cond ((eq todo-type 'done)
                                (car (+org-get-todo-keywords-for todo-keyword)))
                               (todo-keyword)
                               ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

(defun +org--get-property (name &optional bound)
  (save-excursion
    (let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
      (goto-char (point-min))
      (when (re-search-forward re bound t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

;;;###autoload
(defun +org-get-global-property (name &optional file bound)
  "Get a document property named NAME (string) from an org FILE (defaults to
current file). Only scans first 2048 bytes of the document."
  (unless bound
    (setq bound 256))
  (if file
      (with-temp-buffer
        (insert-file-contents-literally file nil 0 bound)
        (+org--get-property name))
    (+org--get-property name bound)))

;;;###autoload
(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))


;;
;;; Modes

;;;###autoload
(define-minor-mode +org-pretty-mode
  "Hides emphasis markers and toggles pretty entities."
  :init-value nil
  :lighter " *"
  :group 'evil-org
  (setq org-hide-emphasis-markers +org-pretty-mode)
  (org-toggle-pretty-entities)
  (with-silent-modifications
   ;; In case the above un-align tables
   (org-table-map-tables 'org-table-align t)))


;;
;;; Commands

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
      (`headline
       (cond ((and (fboundp 'toc-org-insert-toc)
                   (member "TOC" (org-get-tags)))
              (toc-org-insert-toc)
              (message "Updating table of contents"))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             ((or (org-element-property :todo-type context)
                  (org-element-property :scheduled context))
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done)
                   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                       'todo)
                 'done)))
             (t
              (+org--refresh-inline-images-in-subtree)
              (org-clear-latex-preview)
              (org-latex-preview '(4)))))

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
                  (bound-and-true-p evil-local-mode))
         (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies nil)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview))

      (`link
       (let* ((lineage (org-element-lineage context '(link) t))
              (path (org-element-property :path lineage)))
         (if (or (equal (org-element-property :type lineage) "img")
                 (and path (image-type-from-file-name path)))
             (+org--refresh-inline-images-in-subtree)
           (org-open-at-point))))

      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (_
       (if (or (org-in-regexp org-ts-regexp-both nil t)
               (org-in-regexp org-tsr-regexp-both nil  t)
               (org-in-regexp org-any-link-re nil t))
           (call-interactively #'org-open-at-point)
         (+org--refresh-inline-images-in-subtree))))))


;; I use this instead of `org-insert-item' or `org-insert-heading' which are too
;; opinionated and perform this simple task incorrectly (e.g. whitespace in the
;; wrong places).
;;;###autoload
(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

;;;###autoload
(defun +org/insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'above)))


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
(defun +org/toggle-clock (arg)
  "Toggles clock on the last clocked item.

Clock out if an active clock is running. Clock in otherwise.

If in an org file, clock in on the item at point. Otherwise clock into the last
task you clocked into.

See `org-clock-out', `org-clock-in' and `org-clock-in-last' for details on how
the prefix ARG changes this command's behavior."
  (interactive "P")
  (if (org-clocking-p)
      (if arg
          (org-clock-cancel)
        (org-clock-out))
    (org-clock-in-last arg)))


;;; Folds
;;;###autoload
(defalias #'+org/toggle-fold #'+org-cycle-only-current-subtree-h)

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
;;; Hooks

;;;###autoload
(defun +org-indent-maybe-h ()
  "Indent the current item (header or item), if possible.
Made for `org-tab-first-hook' in evil-mode."
  (interactive)
  (cond ((not (and (bound-and-true-p evil-local-mode)
                   (evil-insert-state-p)))
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
         t)
        ((and (save-excursion
                (skip-chars-backward " \t")
                (bolp))
              (org-in-subtree-not-table-p))
         (call-interactively #'tab-to-tab-stop)
         t)))

;;;###autoload
(defun +org-update-cookies-h ()
  "Update counts in headlines (aka \"cookies\")."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let (org-hierarchical-todo-statistics)
      (org-update-parent-todo-statistics))))

;;;###autoload
(defun +org-yas-expand-maybe-h ()
  "Tries to expand a yasnippet snippet, if one is available. Made for
`org-tab-first-hook'."
  (when (bound-and-true-p yas-minor-mode)
    (let ((major-mode (if (org-in-src-block-p t)
                          (org-src-get-lang-mode (org-eldoc-get-src-lang))
                        major-mode))
          (org-src-tab-acts-natively nil) ; causes breakages
          ;; Smart indentation doesn't work with yasnippet, and painfully slow
          ;; in the few cases where it does.
          (yas-indent-line 'fixed))
      ;; HACK Yasnippet field overlays break org-bullet-mode. Don't ask me why.
      (add-hook! 'yas-after-exit-snippet-hook :local
        (when (bound-and-true-p org-bullets-mode)
          (org-bullets-mode -1)
          (org-bullets-mode +1)))
      (cond ((and (or (not (bound-and-true-p evil-local-mode))
                      (evil-insert-state-p))
                  (yas--templates-for-key-at-point))
             (yas-expand)
             t)
            ((use-region-p)
             (yas-insert-snippet)
             t)))))

;;;###autoload
(defun +org-cycle-only-current-subtree-h (&optional arg)
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
(defun +org-clear-babel-results-h ()
  "Remove the results block for the org babel block at point."
  (when (and (org-in-src-block-p t)
             (org-babel-where-is-src-block-result))
    (org-babel-remove-result)
    t))

;;;###autoload
(defun +org-unfold-to-2nd-level-or-point-h ()
  "My version of the 'overview' #+STARTUP option: expand first-level headings.
Expands the first level, but no further. If point was left somewhere deeper,
unfold to point on startup."
  (unless org-agenda-inhibit-startup
    (when (eq org-startup-folded t)
      (outline-hide-sublevels +org-initial-fold-level))
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree))))))

;;;###autoload
(defun +org-remove-occur-highlights-h ()
  "Remove org occur highlights on ESC in normal mode."
  (when org-occur-highlights
    (org-remove-occur-highlights)
    t))

;;;###autoload
(defun +org-enable-auto-update-cookies-h ()
  "Update statistics cookies when saving or exiting insert mode (`evil-mode')."
  (when (bound-and-true-p evil-local-mode)
    (add-hook 'evil-insert-state-exit-hook #'+org-update-cookies-h nil t))
  (add-hook 'before-save-hook #'+org-update-cookies-h nil t))
