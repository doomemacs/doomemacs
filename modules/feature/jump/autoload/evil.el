;;; feature/jump/autoload/evil.el

;;;###autoload (autoload '+jump:online "feature/jump/autoload/evil" nil t)
(evil-define-command +jump:online (query &optional bang)
  "Look up QUERY online. You can prefix your queries with a one-letter shortcut
key (dictated by `+jump-search-url-alist'), otherwise you will be prompted for
what search engine to use."
  (interactive "<a><!>")
  (let ((query query)
        (engine (assoc (car-safe (split-string query " " t t))
                       +jump-search-url-alist)))
    (if engine
        (setq query (string-join (cdr-safe (split-string query " " t t)) " "))
      (let ((engine (completing-read "Search on: "
                                     (mapcar #'cadr +jump-search-url-alist)
                                     nil t)))
        (setq engine (cl-find-if (lambda (x) (equal (cadr x) engine))
                                 +jump-search-url-alist))))
    (unless engine
      (error "Search engine couldn't be found"))
    (+jump/online engine query)))
