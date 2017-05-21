;;; feature/jump/autoload.el

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
                (let ((bounds (bounds-of-thing-at-point 'symbol))
                      (orig-pt (point)))
                  (evil-goto-definition)
                  (let ((pt (point)))
                    (not (and (>= pt (car bounds))
                              (<  pt (cdr bounds))))))))

          (t (user-error "Couldn't find '%s'" sym)))))

;;;###autoload
(defun +jump/references ()
  "Show a list of references to the symbol at point.

Tries `xref-find-references' and falls back to rg/ag."
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (cond ((progn
             (ignore-errors (xref-find-references sym)
                            t)))

          ((and sym
                (featurep 'counsel)
                (let ((regex (rxt-quote-pcre sym)))
                  (or (and (executable-find "rg")
                           (counsel-rg regex (doom-project-root)))
                      (and (executable-find "ag")
                           (counsel-ag regex (doom-project-root)))))))

          (t (error "Couldn't find '%s'" sym)))))

(defvar +jump--online-last-url nil)

;;;###autoload
(defun +jump/online (where search)
  "Looks up SEARCH online, in you browser, as dictated by WHERE.

Interactively, you are prompted to choose a source from
`+jump-search-url-alist'."
  (interactive
   (list (completing-read "Search on: "
                          (mapcar #'cdr +jump-search-url-alist)
                          nil t)
         (or (and (not current-prefix-arg)
                  +jump--online-last-url)
             (thing-at-point 'symbol t))))
  (let ((url (cdr (assoc where +jump-search-url-alist))))
    (when (or (functionp url) (symbolp url))
      (setq url (funcall url)))
    (cl-assert (stringp url))
    (cl-assert (not (string-empty-p url)))
    (cl-assert (not (string-empty-p search)))
    (setq +jump--online-last-url url)
    (browse-url (format url (url-encode-url search)))))

