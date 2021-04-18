;;; tools/lookup/autoload/online.el -*- lexical-binding: t; -*-

(defvar +lookup--last-provider nil)

(defun +lookup--online-provider (&optional force-p namespace)
  (let ((key (or namespace major-mode)))
    (or (and (not force-p)
             (cdr (assq key +lookup--last-provider)))
        (when-let (provider
                   (completing-read
                    "Search on: "
                    (mapcar #'car +lookup-provider-url-alist)
                    nil t))
          (setf (alist-get key +lookup--last-provider) provider)
          provider))))

;;;###autoload
(defun +lookup-online-backend-fn (identifier)
  "Open the browser and search for IDENTIFIER online.
When called for the first time, or with a non-nil prefix argument, prompt for
the search engine to use."
  (+lookup/online
   identifier
   (+lookup--online-provider (not current-prefix-arg))))

;;;###autoload
(defun +lookup/online (query provider)
  "Look up QUERY in the browser using PROVIDER.
When called interactively, prompt for a query and, when called for the first
time, the provider from `+lookup-provider-url-alist'. In subsequent calls, reuse
the previous provider. With a non-nil prefix argument, always prompt for the
provider.

QUERY must be a string, and PROVIDER must be a key of
`+lookup-provider-url-alist'."
  (interactive
   (list (if (use-region-p) (doom-thing-at-point-or-region))
         (+lookup--online-provider current-prefix-arg)))
  (let ((backend (cl-find-if (lambda (x) (or (stringp x) (fboundp x)))
                             (cdr (assoc provider +lookup-provider-url-alist)))))
    (unless (and (functionp backend)
                 (funcall backend query))
      (unless backend
        (user-error "%S is an invalid query engine backend for %S provider"
                    backend provider))
      (cl-check-type backend (or string function))
      (condition-case-unless-debug e
          (progn
            (unless query
              (setq query
                    (read-string (format "Search for (on %s): " provider)
                                 (thing-at-point 'symbol t))))
            (when (or (functionp backend) (symbolp backend))
              (setq backend (funcall backend)))
            (when (string-empty-p query)
              (user-error "The query query is empty"))
            (funcall +lookup-open-url-fn (format backend (url-encode-url query))))
        (error
         (setq +lookup--last-provider
               (delq (assq major-mode +lookup--last-provider)
                     +lookup--last-provider))
         (signal (car e) (cdr e)))))))

;;;###autoload
(defun +lookup/online-select ()
  "Run `+lookup/online', but always prompt for the provider to use."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'+lookup/online)))


;;
;;; Special provider frontends

(defvar ivy-initial-inputs-alist)
(defvar counsel-search-engine)
;;;###autoload
(defun +lookup--online-backend-google (query)
  "Search Google, starting with QUERY, with live autocompletion."
  (cond ((fboundp 'counsel-search)
         (let ((ivy-initial-inputs-alist `((t . ,query)))
               (counsel-search-engine 'google))
           (call-interactively #'counsel-search)
           t))
        ((require 'helm-net nil t)
         (helm :sources 'helm-source-google-suggest
               :buffer "*helm google*"
               :input query)
         t)))

;;;###autoload
(defun +lookup--online-backend-duckduckgo (query)
  "Search DuckDuckGo, starting with QUERY, with live autocompletion."
  (cond ((fboundp 'counsel-search)
         (let ((ivy-initial-inputs-alist `((t . ,query)))
               (counsel-search-engine 'ddg))
           (call-interactively #'counsel-search)
           t))))
