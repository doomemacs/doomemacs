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

(provide 'defuns-org)
;;; defuns-org.el ends here
