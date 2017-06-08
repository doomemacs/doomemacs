;;; feature/jump/autoload.el -*- lexical-binding: t; -*-

(defvar +jump--rg-installed-p (executable-find "rg"))
(defvar +jump--ag-installed-p (executable-find "ag"))

;;;###autoload
(defun +jump/definition (&optional other-window)
  "Jump to the definition of the symbol at point.

Tries xref and falls back to `dumb-jump', then rg/ag, then
`evil-goto-definition' (if evil is active)."
  (interactive "P")
  (let ((sym (thing-at-point 'symbol t))
        successful)
    (cond ((null sym)
           (user-error "Nothing under point"))

          ((ignore-errors (if other-window
                              (xref-find-definitions-other-window sym)
                            (xref-find-definitions sym))
                          t))

          ((and (fboundp 'dumb-jump-go)
                ;; dumb-jump doesn't tell us if it succeeded or not
                (cl-letf (((symbol-function 'dumb-jump-result-follow)
                           `(lambda (result &optional use-tooltip proj)
                              (setq successful t)
                              (,(symbol-function 'dumb-jump-result-follow)
                               result use-tooltip proj))))
                  (if other-window
                      (dumb-jump-go-other-window)
                    (dumb-jump-go))
                  successful)))

          ((and sym
                (featurep 'counsel)
                (let ((regex (rxt-quote-pcre sym)))
                  (or (and +jump--rg-installed-p
                           (counsel-rg regex (doom-project-root)))
                      (and +jump--ag-installed-p
                           (counsel-ag regex (doom-project-root)))))))

          ((and (featurep 'evil)
                evil-mode
                (destructuring-bind (beg end) (bounds-of-thing-at-point 'symbol)
                  (evil-goto-definition)
                  (let ((pt (point)))
                    (not (and (>= pt beg)
                              (<  pt end)))))))

          (t (user-error "Couldn't find '%s'" sym)))))

;;;###autoload
(defun +jump/references ()
  "Show a list of references to the symbol at point.

Tries `xref-find-references' and falls back to rg/ag."
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (cond ((ignore-errors (xref-find-references sym)
                          t))

          ((and sym
                (featurep 'counsel)
                (let ((regex (rxt-quote-pcre sym)))
                  (or (and (executable-find "rg")
                           (counsel-rg regex (doom-project-root)))
                      (and (executable-find "ag")
                           (counsel-ag regex (doom-project-root)))))))

          (t (error "Couldn't find '%s'" sym)))))

(defvar +jump--online-last nil)

;;;###autoload
(defun +jump/online (where search)
  "Looks up SEARCH online, in you browser, as dictated by WHERE.

Interactively, you are prompted to choose a source from
`+jump-search-url-alist'."
  (interactive
   (list (or (and (not current-prefix-arg)
                  +jump--online-last)
             (completing-read (format "Search on (%s): " (thing-at-point 'symbol t))
                              (mapcar #'car +jump-search-url-alist)
                              nil t))
         (thing-at-point 'symbol t)))
  (condition-case _ex
      (let ((url (cdr (assoc where +jump-search-url-alist))))
        (unless url
          (error "'%s' is an invalid search engine" where))
        (when (or (functionp url) (symbolp url))
          (setq url (funcall url)))
        (cl-assert (and (stringp url) (not (string-empty-p url))))
        (when (string-empty-p search)
          (user-error "The search query is empty"))
        (setq +jump--online-last where)
        (browse-url (format url (url-encode-url search))))
    ('error (setq +jump--online-last nil))))

