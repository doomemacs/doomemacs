;;; completion/ivy/autoload/evil.el

(defvar +ivy--file-last-search nil)

;;;###autoload (autoload '+ivy:file-search "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:file-search (beg end search regex-p &optional dir)
  "Preform a `counsel-rg' search with SEARCH. If SEARCH is nil and in visual
mode, use the selection, otherwise activate live ag searching in helm.

If REGEX-P is non-nil, SEARCH will be treated as a regular expression.
DIR specifies the default-directory from which ag is run.

If there is no selection and SEARCH is empty, then relaunch the previous search
session."
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (let ((search (or search
                    (and (evil-visual-state-p)
                         (and beg end (buffer-substring-no-properties beg end)))
                    +ivy--file-last-search)))
    (setq +ivy--file-last-search search)
    (counsel-rg search
                (or dir (doom-project-root))
                (unless regex-p " -F")
                (format "File search (%s)" (if regex-p "regex" "literal")))))

;;;###autoload (autoload '+ivy:file-search-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:file-search-cwd (beg end search regex-p)
  "Perform a `counsel-rg' search for SEARCH (or the current selection) in
`default-directory'."
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (+ivy:file-search beg end search regex-p default-directory))

;;;###autoload (autoload '+ivy:swiper "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper (or search (thing-at-point 'symbol))))
