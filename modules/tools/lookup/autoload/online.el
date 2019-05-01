;;; tools/lookup/autoload/online.el -*- lexical-binding: t; -*-

(defvar +lookup--last-provider nil)

(defun +lookup--online-provider (&optional force-p namespace)
  (let ((key (or namespace major-mode)))
    (or (and (not force-p)
             (cdr (assq key +lookup--last-provider)))
        (when-let* ((provider
                     (completing-read
                      "Search on: "
                      (mapcar #'car +lookup-provider-url-alist)
                      nil t)))
          (setf (alist-get key +lookup--last-provider) provider)
          provider))))

;;;###autoload
(defun +lookup-online-backend (identifier)
  "Opens the browser and searches for IDENTIFIER online.

Will prompt for which search engine to use the first time (or if the universal
argument is non-nil)."
  (+lookup/online
   identifier
   (+lookup--online-provider (not current-prefix-arg))))

;;;###autoload
(defun +lookup/online (search &optional provider)
  "Looks up SEARCH (a string) in you browser using PROVIDER.

PROVIDER should be a key of `+lookup-provider-url-alist'.

When used interactively, it will prompt for a query and, for the first time, the
provider from `+lookup-provider-url-alist'. On consecutive uses, the last
provider will be reused. If the universal argument is supplied, always prompt
for the provider."
  (interactive
   (let ((provider (+lookup--online-provider current-prefix-arg)))
     (list (or (and (use-region-p)
                    (buffer-substring-no-properties (region-beginning)
                                                    (region-end)))
               (read-string (format "Search for (on %s): " provider)
                            (thing-at-point 'symbol t)))
           provider)))
  (condition-case-unless-debug e
      (let ((url (cdr (assoc provider +lookup-provider-url-alist))))
        (unless url
          (user-error "'%s' is an invalid search engine" provider))
        (when (or (functionp url) (symbolp url))
          (setq url (funcall url)))
        (cl-assert (stringp url))
        (when (string-empty-p search)
          (user-error "The search query is empty"))
        (funcall +lookup-open-url-fn (format url (url-encode-url search))))
    (error
     (setq +lookup--last-provider
           (delq (assq major-mode +lookup--last-provider)
                 +lookup--last-provider))
     (signal (car e) (cdr e)))))

;;;###autoload
(defun +lookup/online-select ()
  "Runs `+lookup/online', but always prompts for the provider to use."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'+lookup/online)))
