;;; defuns-org.el

;;;###autoload
(defun narf/org-find-file-in-notes ()
  (interactive)
  (in! (f-slash org-directory)
    (helm-projectile-find-file)))

;;;###autoload
(defun narf/org-find-file ()
  (interactive)
  (in! (f-slash org-directory)
    (helm-find-files nil)))

;;;###autoload
(defun narf/org-find-exported-file ()
  (interactive)
  (in! (f-slash narf-org-export-directory)
    (helm-find-files nil)))

;;;###autoload
(defun narf/org-get-property (name)
  (interactive)
  (save-excursion
    (goto-char 1)
    (re-search-forward (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name)) nil t)
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

;;;###autoload
(defun narf/org-indent ()
  (interactive)
  (cond
   ((and (org-on-heading-p)
         (looking-back "^\\*+ +" (line-beginning-position)))
    (ignore-errors
      (org-demote)))
   (t (call-interactively 'self-insert-command))))

;;;###autoload
(defun narf/org-dedent ()
  (interactive)
  (cond
   ((and (org-on-heading-p)
         (looking-back "^\\*+ +" (line-beginning-position)))
    (ignore-errors
      (org-promote)))
   (t (call-interactively 'self-insert-command))))

;;;###autoload
(defun narf/org-insert-item (direction)
  "Inserts a new heading or item, depending on the context. I use this instead of
`org-insert-item' or `org-insert-heading' because they try to do too much and end up doing
this otherwise simple task wrong (e.g. whitespace in the wrong places)."
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
             ('below
              (org-table-insert-row))
             ('above
              (narf/org-table-prepend-row-or-shift-up))))
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
(defun narf/org-toggle-checkbox ()
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
(defun narf/org-dwim-at-point ()
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
            (narf/org-refresh-inline-images)
          (org-open-at-point))))

     (t (narf/org-refresh-inline-images)))
    (set-window-start nil scroll-pt)))

;;;###autoload
(defun narf/org-refresh-inline-images ()
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

;; Formatting shortcuts
;;;###autoload
(defun narf/org-surround (delim)
  (if (region-active-p)
      (save-excursion
        (goto-char (region-beginning))
        (insert delim)
        (goto-char (region-end))
        (insert delim))
    (insert delim)
    (save-excursion (insert delim))))

;;;###autoload
(defun narf/org-word-count (beg end &optional count-footnotes?)
  "Report the number of words in the Org mode buffer or selected region.
Ignores:
- comments
- tables
- source code blocks (#+BEGIN_SRC ... #+END_SRC, and inline blocks)
- hyperlinks (but does count words in hyperlink descriptions)
- tags, priorities, and TODO keywords in headers
- sections tagged as 'not for export'.

The text of footnote definitions is ignored, unless the optional argument
COUNT-FOOTNOTES? is non-nil."
  (interactive "r")
  (unless mark-active
    (setf beg (point-min)
          end (point-max)))
  (let ((wc 0))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (cond
         ;; Ignore comments.
         ((or (org-at-comment-p) (org-at-table-p))
          nil)
         ;; Ignore hyperlinks. But if link has a description, count
         ;; the words within the description.
         ((looking-at org-bracket-link-analytic-regexp)
          (when (match-string-no-properties 5)
            (let ((desc (match-string-no-properties 5)))
              (save-match-data
                (incf wc (length (remove "" (org-split-string
                                             desc "\\W")))))))
          (goto-char (match-end 0)))
         ((looking-at org-any-link-re)
          (goto-char (match-end 0)))
         ;; Ignore source code blocks.
         ((org-between-regexps-p "^#\\+BEGIN_SRC\\W" "^#\\+END_SRC\\W")
          nil)
         ;; Ignore inline source blocks, counting them as 1 word.
         ((save-excursion
            (backward-char)
            (looking-at org-babel-inline-src-block-regexp))
          (goto-char (match-end 0))
          (setf wc (+ 2 wc)))
         ;; Ignore footnotes.
         ((and (not count-footnotes?)
               (or (org-footnote-at-definition-p)
                   (org-footnote-at-reference-p)))
          nil)
         (t
          (let ((contexts (org-context)))
            (cond
             ;; Ignore tags and TODO keywords, etc.
             ((or (assoc :todo-keyword contexts)
                  (assoc :priority contexts)
                  (assoc :keyword contexts)
                  (assoc :checkbox contexts))
              nil)
             ;; Ignore sections marked with tags that are
             ;; excluded from export.
             ((assoc :tags contexts)
              (if (intersection (org-get-tags-at) org-export-exclude-tags
                                :test 'equal)
                  (org-forward-same-level 1)
                nil))
             (t
              (incf wc))))))
        (re-search-forward "\\w+\\W*")))
    (message (format "%d words in %s." wc
                     (if mark-active "region" "buffer")))))

;;;###autoload
(defun narf/org-remove-link ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
            (description (if (match-end 3)
                             (org-match-string-no-properties 3)
                           (org-match-string-no-properties 1))))
        (apply 'delete-region remove)
        (insert description))))

;;;###autoload
(defun narf/org-table-next-row ()
  (interactive)
  (if (org-at-table-p) (org-table-next-row) (org-down-element)))

;;;###autoload
(defun narf/org-table-previous-row ()
  (interactive)
  (if (org-at-table-p) (narf--org-table-previous-row) (org-up-element)))

;;;###autoload
(defun narf/org-table-next-field ()
  (interactive)
  (if (org-at-table-p) (org-table-next-field) (org-end-of-line)))

;;;###autoload
(defun narf/org-table-previous-field ()
  (interactive)
  (if (org-at-table-p) (org-table-previous-field) (org-beginning-of-line)))

;;;###autoload
(defun narf/org-table-append-field-or-shift-right ()
  (interactive)
  (org-shiftmetaright)
  (when (org-at-table-p) (org-metaright)))

;;;###autoload
(defun narf/org-table-prepend-field-or-shift-left ()
  (interactive)
  (if (org-at-table-p)
      (org-shiftmetaright)
    (org-shiftmetaleft)))

;;;###autoload
(defun narf/org-table-append-row-or-shift-down ()
  (interactive)
  (org-shiftmetadown)
  (when (org-at-table-p) (org-metadown)))

;;;###autoload
(defun narf/org-table-prepend-row-or-shift-up ()
  (interactive)
  (if (org-at-table-p)
      (org-shiftmetadown)
    (org-shiftmetaup)))

(defun narf--org-table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
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

;;;###autoload (autoload 'narf:org-link "defuns-org" nil t)
(evil-define-command narf:org-link (link)
  (interactive "<a>")
  (let ((beg evil-visual-beginning)
        (end evil-visual-end))
    (org-insert-link nil link (when (and beg end) (buffer-substring-no-properties beg end)))))

(provide 'defuns-org)
;;; defuns-org.el ends here
