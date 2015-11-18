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
         (org-insert-heading-respect-content))
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

;;;###autoload
(defun narf/org-execute-at-point ()
  (interactive)
  (let* ((context (org-element-lineage
                   (org-element-context)
                   '(table table-row clock comment comment-block footnote-definition
                     footnote-reference headline inlinetask keyword link
                     latex-fragment src-block item plain-list timestamp babel-call)
                   t))
         (type (org-element-type context))
         (value (org-element-property :value context)))
    (cond
     ((memq type '(table table-row))
      (if (org-element-property :tblfm (org-element-property :parent context))
          (org-table-recalculate t)
        (org-table-align)))

     ((and (memq type '(item))
           (org-element-property :checkbox context))
      (org-toggle-checkbox))

     ((and (memq type '(headline))
           (org-element-property :todo-type context))
      (if (eq (org-element-property :todo-type context) 'done)
          (org-todo 'todo)
        (org-todo 'done)))

     ((memq type '(babel-call))
      (org-babel-lob-execute-maybe))

     ((memq type '(src-block))
      (org-babel-execute-src-block))

     ((memq type '(latex-fragment))
      (org-toggle-latex-fragment))

     ((memq type '(link))
      (org-open-at-point))

     (t (org-toggle-inline-images)))))

;;;###autoload
(defun narf/org-toggle-inline-images-at-point ()
  (interactive)
  (if (> (length org-inline-image-overlays) 0)
      (org-remove-inline-images)
    (org-display-inline-images nil t (line-beginning-position) (line-end-position))))

;; Formatting shortcuts
;;;###autoload
(defun narf/org-surround (delim)
  (insert delim) (save-excursion (insert delim)))

;;;###autoload (autoload 'narf:org-search-files-or-headers "defuns-org" nil t)
(evil-define-command narf:org-search-files-or-headers (&optional bang)
  (interactive "<!>")
  (require 'org)
  (if bang
      (ido-find-file-in-dir org-directory)
    (call-interactively 'helm-org-agenda-files-headings)))

;;;###autoload
(defun narf:org-list-attachments ()
  "Find files in org-attachment directory"
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (files (find-lisp-find-files org-attach-directory "."))
         (file-assoc-list
          (mapcar (lambda (x)
                    (cons (file-name-nondirectory x)
                          x))
                  files))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ido-completing-read "Org attachments: " filename-list nil t))
         (longname (cdr (assoc filename file-assoc-list))))
    (ido-set-current-directory
     (if (file-directory-p longname)
         longname
       (file-name-directory longname)))
    (setq ido-exit 'refresh
          ido-text-init ido-text
          ido-rotate-temp t)
    (exit-minibuffer)))

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

;;;###autoload (autoload 'narf:org-attach "defuns-org" nil t)
(evil-define-command narf:org-attach (&optional link)
  (interactive "<a>")
  (require 'org-attach)
  (let ((path ".attach")
        (new-name (concat (int-to-string (truncate (float-time))) "-" (f-filename link)))
        new-path)
    (unless (file-exists-p path)
      (make-directory path))
    (when path
      (setq new-path (format "%s/%s" path new-name))
      (cond ((string-match-p "^https?://" link)
             (url-copy-file link new-path))
            (t (copy-file link new-path)))
      (insert (format "[[./%s]]" (abbreviate-file-name new-path))))))

;;;###autoload (autoload 'narf:org-export "defuns-org" nil t)
(evil-define-command narf:org-export (dest)
  (interactive "<a>")
  (let ((path (if (string-match-p "^[/~]" dest)
                  dest
                (expand-file-name dest default-directory))))
    (shell-command
     (format "/usr/local/bin/pandoc '%s' -o '%s'"
             (buffer-file-name) path))
    (message "Done! Exported to: %s" path)))

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
(defun narf/org-table-append-row-or-shift-right ()
  (interactive)
  (org-shiftmetaright)
  (when (org-at-table-p) (org-metaright)))

;;;###autoload
(defun narf/org-table-prepend-row-or-shift-left ()
  (interactive)
  (if (org-at-table-p)
      (org-shiftmetaright)
    (org-shiftmetaleft)))

;;;###autoload
(defun narf/org-table-append-field-or-shift-down ()
  (interactive)
  (org-shiftmetadown)
  (when (org-at-table-p) (org-metadown)))

;;;###autoload
(defun narf/org-table-prepend-field-or-shift-up ()
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

;;;###autoload
(defun narf/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
            (description (if (match-end 3)
                             (org-match-string-no-properties 3)
                           (org-match-string-no-properties 1))))
        (apply 'delete-region remove)
        (insert description))))

(provide 'defuns-org)
;;; defuns-org.el ends here
