;;; completion/ivy/autoload/evil.el

(defvar +ivy--file-last-search nil)

;;;###autoload (autoload '+ivy:file-search "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:file-search (beg end query all-files-p &optional dir)
  "Preform a `counsel-rg' search with QUERY. If QUERY is nil and in visual mode,
use the selection, otherwise activate live rg searching in ivy.

If ALL-FILES-P is non-nil, don't respect .gitignore files and search everything.

If there is no selection and QUERY is empty, then relaunch the previous search
session."
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (let ((query (or query
                    (and (evil-visual-state-p)
                         (and beg end
                              (rxt-quote-pcre (buffer-substring-no-properties beg end))))
                    +ivy--file-last-search))
        ;; smart-case instead of case-insensitive flag
        (counsel-rg-base-command
         (replace-regexp-in-string " -i " " -S " counsel-rg-base-command)))
    (setq +ivy--file-last-search query)
    (counsel-rg query
                (or dir (doom-project-root))
                (when all-files-p " -u")
                (format "File search%s" (if all-files-p " (all)" "")))))

;;;###autoload (autoload '+ivy:file-search-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:file-search-cwd (beg end search all-files-p)
  "Perform a `counsel-rg' search for SEARCH (or the current selection) in
`default-directory'."
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (+ivy:file-search beg end search all-files-p default-directory))

;;;###autoload (autoload '+ivy:swiper "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper search))

;;;###autoload (autoload '+ivy:todo "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:todo (&optional bang)
  "An ex wrapper around `+ivy/tasks'."
  (interactive "<!>")
  (+ivy/tasks bang))
