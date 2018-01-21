;;; feature/lookup/autoload/lookup.el -*- lexical-binding: t; -*-

(defvar +lookup--rg-installed-p (executable-find "rg"))
(defvar +lookup--ag-installed-p (executable-find "ag"))
(defvar +lookup--last-provider nil)

;; Helpers
(defun +lookup--online-provider (&optional force-p namespace)
  (let ((key (or namespace major-mode)))
    (or (and (not force-p)
             (cdr (assq key +lookup--last-provider)))
        (when-let* ((provider
                     (completing-read
                      "Search on: "
                      (mapcar #'car +lookup-provider-url-alist)
                      nil t)))
          (map-put +lookup--last-provider key provider)
          provider))))

(defun +lookup--symbol-or-region (&optional initial)
  (cond (initial)
        ((use-region-p)
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
        ((thing-at-point 'symbol t))))

(defun +lookup--jump-to (prop identifier &optional other-window)
  (with-selected-window
      (if other-window
          (save-excursion (other-window 1) (selected-window))
        (selected-window))
    (let ((fn (plist-get +lookup-current-functions prop))
          (origin (point-marker)))
      (condition-case e
          (or (if (commandp fn)
                  (call-interactively fn)
                (funcall fn identifier))
              (/= (point-marker) origin))
        ('error
         (message "%s" e)
         nil)))))


;;
;; Main commands
;;

;;;###autoload
(defun +lookup/definition (identifier &optional other-window)
  "Jump to the definition of the symbol at point. It will try several things
to find it:

1. It will try whatever function that has been set for the current buffer, in
   `+lookup-current-functions'.
2. Then try any available xref backends,
3. Then `dumb-jump',
4. Then a plain project-wide text search, using ripgrep or the_silver_searcher.
5. Then, if `evil-mode' is active, use `evil-goto-definition',

Failing all that, it will give up with an error."
  (interactive
   (list (thing-at-point 'symbol t)
         current-prefix-arg))
  (cond ((null identifier)
         (user-error "Nothing under point"))

        ((and (plist-member +lookup-current-functions :definition)
              (+lookup--jump-to :definition identifier)))

        ((ignore-errors (if other-window
                            (xref-find-definitions-other-window identifier)
                          (xref-find-definitions identifier))
                        t))

        ((and (require 'dumb-jump nil t)
              ;; dumb-jump doesn't tell us if it succeeded or not
              (let ((old-fn (symbol-function 'dumb-jump-get-results))
                    successful)
                (cl-letf (((symbol-function 'dumb-jump-get-results)
                           (lambda (&optional prompt)
                             (let* ((plist (funcall old-fn prompt))
                                    (results (plist-get plist :results)))
                               (when (and results (> (length results) 0))
                                 (setq successful t))
                               plist))))
                  (if other-window
                      (dumb-jump-go-other-window)
                    (dumb-jump-go))
                  successful))))

        ((and identifier
              (featurep 'counsel)
              (let ((regex (rxt-quote-pcre identifier)))
                (or (and +lookup--rg-installed-p
                         (counsel-rg regex (doom-project-root)))
                    (and +lookup--ag-installed-p
                         (counsel-ag regex (doom-project-root)))))))

        ((and (featurep 'evil)
              evil-mode
              (cl-destructuring-bind (beg . end)
                  (bounds-of-thing-at-point 'symbol)
                (evil-goto-definition)
                (let ((pt (point)))
                  (not (and (>= pt beg)
                            (<  pt end)))))))

        (t (user-error "Couldn't find '%s'" identifier))))

;;;###autoload
(defun +lookup/references (identifier)
  "Show a list of references to the symbol at point.

Tries `xref-find-references' and falls back to rg/ag."
  (interactive (list (thing-at-point 'symbol t)))
  (cond ((and (plist-member +lookup-current-functions :references)
              (+lookup--jump-to :references identifier)))

        ((ignore-errors (xref-find-references identifier)
                        t))

        ((and identifier
              (featurep 'counsel)
              (let ((regex (rxt-quote-pcre identifier)))
                (or (and (executable-find "rg")
                         (counsel-rg regex (doom-project-root)))
                    (and (executable-find "ag")
                         (counsel-ag regex (doom-project-root)))))))

        (t (error "Couldn't find '%s'" identifier))))

;;;###autoload
(defun +lookup/documentation (identifier)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

Goes down a list of possible backends:

1. The :documentation spec defined with by `doom--set:lookup'
2. If the +docsets flag is active for :feature lookup, use `+lookup/in-docsets'
3. If the +devdocs flag is active for :feature lookup, run `+lookup/in-devdocs'
4. Fall back to an online search, with `+lookup/online'"
  (interactive
   (list (+lookup--symbol-or-region)))
  (cond ((and (plist-member +lookup-current-functions :documentation)
              (+lookup--jump-to :documentation identifier)))

        ((and (featurep! :feature lookup +docsets)
              (cl-find-if #'helm-dash-docset-installed-p
                          (or (bound-and-true-p counsel-dash-docsets)
                              (bound-and-true-p helm-dash-docsets))))
         (+lookup/in-docsets identifier))

        ((featurep! :feature lookup +devdocs)
         (+lookup/in-devdocs identifier))

        ((+lookup/online
          identifier
          (+lookup--online-provider (not current-prefix-arg))))))


;;
;; Source-specific commands
;;

;;;###autoload
(defun +lookup/in-devdocs (&optional query docs)
  "TODO"
  (interactive)
  (require 'devdocs)
  (let* ((docs
          (unless (eq docs 'blank)
            (or docs (cdr (assq major-mode devdocs-alist)) "")))
         (query (or query (+lookup--symbol-or-region) ""))
         (pattern (string-trim-left (format "%s %s" docs query))))
    (unless (and current-prefix-arg docs)
      (setq pattern (read-string "Lookup on devdocs.io: " pattern)))
    (funcall +lookup-open-url-fn
             (format "%s/#q=%s" devdocs-url
                     (url-hexify-string pattern)))
    (unless (string-empty-p pattern)
      (cl-pushnew pattern devdocs-search-history))))

(defvar counsel-dash-docsets)
(defvar helm-dash-docsets)
;;;###autoload
(defun +lookup/in-docsets (&optional query docsets)
  "TODO"
  (interactive)
  (let* ((counsel-dash-docsets
          (unless (eq docsets 'blank)
            (or docsets
                (or (bound-and-true-p counsel-dash-docsets)
                    (bound-and-true-p helm-dash-docsets)))))
         (helm-dash-docsets counsel-dash-docsets)
         (query (or query (+lookup--symbol-or-region) "")))
    (cond ((featurep! :completion helm)
           (helm-dash query))
          ((featurep! :completion ivy)
           (counsel-dash query))
          (t
           (user-error "No dash backend is installed, enable ivy or helm.")))))

;;;###autoload
(defun +lookup/online (search &optional provider)
  "Looks up SEARCH (a string) in you browser using PROVIDER.

PROVIDER should be a key of `+lookup-provider-url-alist'.

When used interactively, it will prompt for a query and, for the first time, the
provider from `+lookup-provider-url-alist'. On consecutive uses, the last
provider will be reused. If the universal argument is supplied, always prompt
for the provider."
  (interactive
   (list (or (and (use-region-p)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end)))
             (read-string "Search for: " (thing-at-point 'symbol t)))
         (+lookup--online-provider current-prefix-arg)))
  (condition-case ex
      (let ((url (cdr (assoc provider +lookup-provider-url-alist))))
        (unless url
          (error "'%s' is an invalid search engine" provider))
        (when (or (functionp url) (symbolp url))
          (setq url (funcall url)))
        (cl-assert (and (stringp url) (not (string-empty-p url))))
        (when (string-empty-p search)
          (user-error "The search query is empty"))
        (funcall +lookup-open-url-fn (format url (url-encode-url search))))
    ('error
     (map-delete +lookup--last-provider major-mode)
     (message "Failed: %s" ex))))

;;;###autoload
(defun +lookup/online-select ()
  "Runs `+lookup/online', but always prompts for the provider to use."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'+lookup/online)))

