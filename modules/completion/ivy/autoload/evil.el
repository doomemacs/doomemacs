;;; completion/ivy/autoload/evil.el

(defvar +ivy--ag-last-search nil)

;;;###autoload (autoload '+ivy:ag-search "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:ag-search (beg end search regex-p &optional dir)
  "Preform a counsel search with SEARCH. If SEARCH is nil and in visual mode,
use the selection, otherwise activate live ag searching in helm.

If REGEX-P is non-nil, SEARCH will be treated as a regular expression.
DIR specifies the default-directory from which ag is run."
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (let ((search (or search
                    (and (evil-visual-state-p)
                         (and beg end
                              (let ((str (buffer-substring-no-properties beg end)))
                                (if regex-p (rxt-quote-pcre str) str))))
                    +ivy--ag-last-search)))
    (setq +ivy--ag-last-search search)
    (counsel-ag (if regex-p search (rxt-quote-pcre search))
                (or dir (doom-project-root))
                (concat "--nocolor --nogroup" (if regex-p " -Q")))))

;;;###autoload (autoload '+ivy:ag-search-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:ag-search-cwd (beg end search regex-p)
  :type inclusive :repeat nil
  (interactive "<r><a><!>")
  (+ivy:ag-search beg end search regex-p default-directory))

;;;###autoload (autoload '+ivy:swiper "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper (or search (thing-at-point 'symbol))))
