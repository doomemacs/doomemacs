;;; feature/jump/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+jump:online "feature/jump/autoload/evil" nil t)
(evil-define-command +jump:online (query &optional bang)
  "Look up QUERY online. Will prompt for search engine the first time, then
reuse it on consecutive uses of this command. If BANG, always prompt for search
engine."
  (interactive "<a><!>")
  (let ((query (or query (thing-at-point 'symbol t))))
    (unless query
      (user-error "The search query is empty"))
    (+jump/online
     (or (and (not bang) (bound-and-true-p +jump--online-last))
         (completing-read (format "Search on (%s): " query)
                          (mapcar #'car +jump-search-url-alist)
                          nil t))
     query)))
