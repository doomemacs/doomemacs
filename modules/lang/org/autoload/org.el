;;; lang/org/autoload/org.el -*- lexical-binding: t; -*-

;;
;;; Helpers

(defun +org--toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree."
  (let* ((beg (or beg
                  (if (org-before-first-heading-p)
                      (save-excursion (point-min))
                    (save-excursion (org-back-to-heading) (point)))))
         (end (or end
                  (if (org-before-first-heading-p)
                      (save-excursion (org-next-visible-heading 1) (point))
                    (save-excursion (org-end-of-subtree) (point)))))
         (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                     (ignore-errors (overlays-in beg end)))))
    (dolist (ov overlays nil)
      (delete-overlay ov)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))

(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
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
         (run-hooks 'org-insert-heading-hook)
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

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
(defun +org/return ()
  "Call `org-return' then indent (if `electric-indent-mode' is on)."
  (interactive)
  (org-return electric-indent-mode))

;;;###autoload
(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
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
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

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
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (org-toggle-checkbox))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

;;;###autoload
(defun +org/shift-return (&optional arg)
  "Insert a literal newline, or dwim in tables.
Executes `org-table-copy-down' if in table."
  (interactive "p")
  (if (org-at-table-p)
      (org-table-copy-down arg)
    (org-return nil arg)))


;; I use these instead of `org-insert-item' or `org-insert-heading' because they
;; impose bizarre whitespace rules depending on cursor location and many
;; settings. These commands have a much simpler responsibility.
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
(defun +org/toggle-last-clock (arg)
  "Toggles last clocked item.

Clock out if an active clock is running (or cancel it if prefix ARG is non-nil).

If no clock is active, then clock into the last item. See `org-clock-in-last' to
see how ARG affects this command."
  (interactive "P")
  (require 'org-clock)
  (cond ((org-clocking-p)
         (if arg
             (org-clock-cancel)
           (org-clock-out)))
        ((and (null org-clock-history)
              (or (org-on-heading-p)
                  (org-at-item-p))
              (y-or-n-p "No active clock. Clock in on current item?"))
         (org-clock-in))
        ((org-clock-in-last arg))))


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

;;;###autoload
(defun +org/close-all-folds (&optional level)
  "Close all folds in the buffer (or below LEVEL)."
  (interactive "p")
  (outline-hide-sublevels (or level 1)))

;;;###autoload
(defun +org/open-all-folds (&optional level)
  "Open all folds in the buffer (or up to LEVEL)."
  (interactive "P")
  (if (integerp level)
      (outline-hide-sublevels level)
    (outline-show-all)))

(defun +org--get-foldlevel ()
  (let ((max 1))
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (org-next-visible-heading 1)
          (when (memq (get-char-property (line-end-position)
                                         'invisible)
                      '(outline org-fold-outline))
            (let ((level (org-outline-level)))
              (when (> level max)
                (setq max level))))))
      max)))

;;;###autoload
(defun +org/show-next-fold-level (&optional count)
  "Decrease the fold-level of the visible area of the buffer. This unfolds
another level of headings on each invocation."
  (interactive "p")
  (let ((new-level (+ (+org--get-foldlevel) (or count 1))))
    (outline-hide-sublevels new-level)
    (message "Folded to level %s" new-level)))

;;;###autoload
(defun +org/hide-next-fold-level (&optional count)
  "Increase the global fold-level of the visible area of the buffer. This folds
another level of headings on each invocation."
  (interactive "p")
  (let ((new-level (max 1 (- (+org--get-foldlevel) (or count 1)))))
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
        ((and (bound-and-true-p org-cdlatex-mode)
              (or (org-inside-LaTeX-fragment-p)
                  (org-inside-latex-macro-p)))
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
         (save-window-excursion
           (org-babel-do-in-edit-buffer
            (call-interactively #'indent-for-tab-command)))
         t)
        ((and (save-excursion
                (skip-chars-backward " \t")
                (bolp))
              (org-in-subtree-not-table-p))
         (call-interactively #'tab-to-tab-stop)
         t)))

;;;###autoload
(defun +org-yas-expand-maybe-h ()
  "Expand a yasnippet snippet, if trigger exists at point or region is active.
Made for `org-tab-first-hook'."
  (when (and (modulep! :editor snippets)
             (require 'yasnippet nil t)
             (bound-and-true-p yas-minor-mode))
    (and (let ((major-mode (cond ((org-in-src-block-p t)
                                  (org-src-get-lang-mode (org-eldoc-get-src-lang)))
                                 ((org-inside-LaTeX-fragment-p)
                                  'latex-mode)
                                 (major-mode)))
               (org-src-tab-acts-natively nil) ; causes breakages
               ;; Smart indentation doesn't work with yasnippet, and painfully slow
               ;; in the few cases where it does.
               (yas-indent-line 'fixed))
           (cond ((and (or (not (bound-and-true-p evil-local-mode))
                           (evil-insert-state-p)
                           (evil-emacs-state-p))
                       (or (and (bound-and-true-p yas--tables)
                                (gethash major-mode yas--tables))
                           (progn (yas-reload-all) t))
                       (yas--templates-for-key-at-point))
                  (yas-expand)
                  t)
                 ((use-region-p)
                  (yas-insert-snippet)
                  t)))
         ;; HACK Yasnippet breaks org-superstar-mode because yasnippets is
         ;;      overzealous about cleaning up overlays.
         (when (bound-and-true-p org-superstar-mode)
           (org-superstar-restart)))))

;;;###autoload
(defun +org-cycle-only-current-subtree-h (&optional arg)
  "Toggle the local fold at the point, and no deeper.
`org-cycle's standard behavior is to cycle between three levels: collapsed,
subtree and whole document. This is slow, especially in larger org buffer. Most
of the time I just want to peek into the current subtree -- at most, expand
*only* the current subtree.

All my (performant) foldings needs are met between this and `org-show-subtree'
(on zO for evil users), and `org-cycle' on shift-TAB if I need it."
  (interactive "P")
  (unless (or (eq this-command 'org-shifttab)
              (and (bound-and-true-p org-cdlatex-mode)
                   (or (org-inside-LaTeX-fragment-p)
                       (org-inside-latex-macro-p))))
    (save-excursion
      (org-beginning-of-line)
      (let (invisible-p)
        (when (and (org-at-heading-p)
                   (or org-cycle-open-archived-trees
                       (not (member org-archive-tag (org-get-tags))))
                   (or (not arg)
                       (setq invisible-p
                             (memq (get-char-property (line-end-position)
                                                      'invisible)
                                   '(outline org-fold-outline)))))
          (unless invisible-p
            (setq org-cycle-subtree-status 'subtree))
          (org-cycle-internal-local)
          t)))))

;;;###autoload
(defun +org-make-last-point-visible-h ()
  "Unfold subtree around point if saveplace places us in a folded region."
  (and (not org-inhibit-startup)
       (not org-inhibit-startup-visibility-stuff)
       ;; Must be done on a timer because `org-show-set-visibility' (used by
       ;; `org-reveal') relies on overlays that aren't immediately available
       ;; when `org-mode' first initializes.
       (let ((buf (current-buffer)))
         (unless (doom-temp-buffer-p buf)
           (run-at-time 0.1 nil (lambda ()
                                  (when (buffer-live-p buf)
                                    (with-current-buffer buf
                                      (org-reveal '(4))))))))))

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
    (add-hook 'evil-insert-state-exit-hook #'org-update-parent-todo-statistics nil t))
  (add-hook 'before-save-hook #'org-update-parent-todo-statistics nil t))
