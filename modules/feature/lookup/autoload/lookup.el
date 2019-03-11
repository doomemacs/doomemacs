;;; feature/lookup/autoload/lookup.el -*- lexical-binding: t; -*-

(defvar +lookup--last-provider nil)

(defvar +lookup--handler-alist nil)

;;;###autodef
(defun set-lookup-handlers! (modes &rest plist)
  "Define a jump target for major MODES.

This overwrites previously defined handlers for MODES. If used on minor modes,
they are combined with handlers defined for other minor modes or the major mode
it's activated in.

This can be passed nil as its second argument to unset handlers for MODES. e.g.

  (set-lookup-handlers! 'python-mode nil)

Otherwise, these properties are available to be set:

:definition FN
  Run when jumping to a symbol's definition.
  Used by `+lookup/definition'.
:references FN
  Run when looking for usage references of a symbol in the current project.
  Used by `+lookup/references'.
:documentation FN
  Run when looking up documentation for a symbol.
  Used by `+lookup/documentation'.
:file FN
  Run when looking up the file for a symbol/string. Typically a file path.
  Used by `+lookup/file'.
:xref-backend FN
  Defines an xref backend for a major-mode. If you define :definition and
  :references along with :xref-backend, those will have higher precedence.
:async BOOL
  Indicates that the supplied handlers *after* this property are asynchronous.
  Note: async handlers do not fall back to the default handlers, due to their
  nature. To get around this, you must write specialized wrappers to wait for
  the async response and return 'fallback.

\(fn MODE-OR-MODES &key ASYNC DEFINITION REFERENCES DOCUMENTATION FILE XREF-BACKEND)"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+lookup|init-%s" mode))))
      (cond ((null (car plist))
             (remove-hook hook fn)
             (setq +lookup--handler-alist
                   (delq (assq mode +lookup--handler-alist)
                         +lookup--handler-alist))
             (unintern fn nil))
            ((let ((old-plist (cdr (assq mode +lookup--handler-alist)))
                   async)
               (while plist
                 (let ((prop (pop plist))
                       (fns (pop plist)))
                   (if (eq prop :async)
                       (setq async t)
                     (dolist (fn (doom-enlist fns))
                       (put fn '+lookup-async async))
                     (setq old-plist (plist-put old-plist prop fns)))))
               (setq plist old-plist)
               (setf (alist-get mode +lookup--handler-alist) plist))
             (fset fn
                   (lambda ()
                     (when (or (eq major-mode mode)
                               (and (boundp mode)
                                    (symbol-value mode)))
                       (cl-destructuring-bind
                           (&key definition references documentation file xref-backend async)
                           plist
                         (when definition
                           (add-hook '+lookup-definition-functions definition nil t))
                         (when references
                           (add-hook '+lookup-references-functions references nil t))
                         (when documentation
                           (add-hook '+lookup-documentation-functions documentation nil t))
                         (when file
                           (add-hook '+lookup-file-functions file nil t))
                         (when xref-backend
                           (add-hook 'xref-backend-functions xref-backend nil t))))))
             (add-hook hook fn))))))


;;
;; Helpers

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
          (setf (alist-get key +lookup--last-provider) provider)
          provider))))

(defun +lookup--symbol-or-region (&optional initial)
  (cond ((stringp initial)
         initial)
        ((use-region-p)
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
        ((require 'xref nil t)
         (xref-backend-identifier-at-point (xref-find-backend)))))

(defun +lookup--run-hooks (hook identifier origin &optional other-window)
  (doom-log "Looking up '%s' with '%s'" identifier hook)
  (condition-case-unless-debug e
      (if (get hook '+lookup-async)
          (progn
            (when other-window
              ;; If async, we can't catch the window change or destination buffer
              ;; reliably, so we set up the new window ahead of time.
              (switch-to-buffer-other-window (current-buffer))
              (goto-char (marker-position origin)))
            (if (commandp hook)
                (call-interactively hook)
              (funcall hook identifier))
            t)
        (save-window-excursion
          (when (or (if (commandp hook)
                        (call-interactively hook)
                      (funcall hook identifier))
                    (null origin)
                    (/= (point-marker) origin))
            (point-marker))))
    ((error user-error)
     (message "%s" e)
     nil)))

(defun +lookup--jump-to (prop identifier &optional other-window)
  (let ((ret
         (condition-case e
             (run-hook-wrapped
              (plist-get (list :definition '+lookup-definition-functions
                               :references '+lookup-references-functions
                               :documentation '+lookup-documentation-functions
                               :file '+lookup-file-functions)
                         prop)
              '+lookup--run-hooks
              identifier
              (point-marker)
              other-window)
           (quit (user-error "Aborted %s lookup" prop)))))
    (cond ((null ret)
           (message "Could not find '%s'" identifier)
           nil)
          ((markerp ret)
           (funcall (if other-window
                        #'switch-to-buffer-other-window
                      #'switch-to-buffer)
                    (marker-buffer ret))
           (goto-char ret)
           (recenter)
           t))))


;;
;; Lookup backends

(defun +lookup-xref-definitions-backend (identifier)
  "Non-interactive wrapper for `xref-find-definitions'"
  (xref-find-definitions identifier))

(defun +lookup-xref-references-backend (identifier)
  "Non-interactive wrapper for `xref-find-references'"
  (xref-find-references identifier))

(defun +lookup-dumb-jump-backend (_identifier)
  "Look up the symbol at point (or selection) with `dumb-jump', which conducts a
project search with ag, rg, pt, or git-grep, combined with extra heuristics to
reduce false positives.

This backend prefers \"just working\" over accuracy."
  (when (require 'dumb-jump nil t)
    ;; dumb-jump doesn't tell us if it succeeded or not
    (plist-get (dumb-jump-go) :results)))

(defun +lookup-project-search-backend (identifier)
  "Conducts a simple project text search for IDENTIFIER.

Uses and requires `+ivy-file-search' or `+helm-file-search'. Will return nil if
neither is available. These search backends will use ag, rg, or pt (in an order
dictated by `+ivy-project-search-engines' or `+helm-project-search-engines',
falling back to git-grep)."
  (unless identifier
    (let ((query (rxt-quote-pcre identifier)))
      (ignore-errors
        (cond ((featurep! :completion ivy)
               (+ivy-file-search nil :query query)
               t)
              ((featurep! :completion helm)
               (+helm-file-search nil :query query)
               t))))))

(defun +lookup-evil-goto-definition-backend (identifier)
  "Uses `evil-goto-definition' to conduct a text search for IDENTIFIER in the
current buffer."
  (and (fboundp 'evil-goto-definition)
       (ignore-errors
         (cl-destructuring-bind (beg . end)
             (bounds-of-thing-at-point 'symbol)
           (evil-goto-definition)
           (let ((pt (point)))
             (not (and (>= pt beg)
                       (<  pt end))))))))

(defun +lookup-dash-docsets-backend (identifier)
  "Looks up IDENTIFIER in available Dash docsets, if any are installed.

Docsets must be installed with `+lookup/install-docset'. These can also be
accessed via `+lookup/in-docsets'."
  (and (featurep! +docsets)
       (or (require 'counsel-dash nil t)
           (require 'helm-dash nil t))
       (let ((docsets (+lookup-docsets-for-buffer)))
         (when (cl-some #'+lookup-docset-installed-p docsets)
           (+lookup/in-docsets identifier docsets)
           t))))

(defun +lookup-online-backend (identifier)
  "Opens the browser and searches for IDENTIFIER online.

Will prompt for which search engine to use the first time (or if the universal
argument is non-nil)."
  (+lookup/online
   identifier
   (+lookup--online-provider (not current-prefix-arg))))


;;
;; Main commands

;;;###autoload
(defun +lookup/definition (identifier &optional other-window)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point).

If OTHER-WINDOW (universal argument), open the result in another window.

Each function in `+lookup-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive
   (list (+lookup--symbol-or-region)
         current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))

        ((+lookup--jump-to :definition identifier other-window))

        ((error "Couldn't find the definition of '%s'" identifier))))

;;;###autoload
(defun +lookup/references (identifier &optional other-window)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)

Tries each function in `+lookup-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive
   (list (+lookup--symbol-or-region)
         current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))

        ((+lookup--jump-to :references identifier other-window))

        ((error "Couldn't find references of '%s'" identifier))))

;;;###autoload
(defun +lookup/documentation (identifier &optional arg)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

First attempts the :documentation handler specified with `set-lookup-handlers!'
for the current mode/buffer (if any), then falls back to the backends in
`+lookup-documentation-functions'."
  (interactive
   (list (+lookup--symbol-or-region)
         current-prefix-arg))
  (cond ((+lookup--jump-to :documentation identifier t))

        ((user-error "Couldn't find documentation for '%s'" identifier))))

(defvar ffap-file-finder)
;;;###autoload
(defun +lookup/file (path)
  "Figure out PATH from whatever is at point and open it.

Each function in `+lookup-file-functions' is tried until one changes the point
or the current buffer.

Otherwise, falls back on `find-file-at-point'."
  (interactive
   (progn
     (require 'ffap)
     (list
      (or (ffap-guesser)
          (ffap-read-file-or-url
           (if ffap-url-regexp "Find file or URL: " "Find file: ")
           (+lookup--symbol-or-region))))))
  (require 'ffap)
  (cond ((not path)
         (call-interactively #'find-file-at-point))

        ((ffap-url-p path)
         (find-file-at-point path))

        ((not (+lookup--jump-to :file path))
         (let ((fullpath (expand-file-name path)))
           (when (and buffer-file-name (file-equal-p fullpath buffer-file-name))
             (user-error "Already here"))
           (let* ((insert-default-directory t)
                  (project-root (doom-project-root))
                  (ffap-file-finder
                   (cond ((not (file-directory-p fullpath))
                          #'find-file)
                         ((file-in-directory-p fullpath project-root)
                          (lambda (dir)
                            (let ((default-directory dir))
                              (without-project-cache!
                               (let ((file (projectile-completing-read "Find file: "
                                                                       (projectile-current-project-files)
                                                                       :initial-input path)))
                                 (find-file (expand-file-name file (doom-project-root)))
                                 (run-hooks 'projectile-find-file-hook))))))
                         (#'doom-project-browse))))
             (find-file-at-point path))))))


;;
;; Source-specific commands

(defvar counsel-dash-docsets)
(defvar helm-dash-docsets)
;;;###autoload
(defun +lookup/in-docsets (&optional query docsets)
  "Looks up QUERY (a string) in available Dash docsets for the current buffer.

DOCSETS is a list of docset strings. Docsets can be installed with
`+lookup/install-docset'."
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
          ((user-error "No dash backend is installed, enable ivy or helm.")))))

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
  (condition-case-unless-debug e
      (let ((url (cdr (assoc provider +lookup-provider-url-alist))))
        (unless url
          (user-error "'%s' is an invalid search engine" provider))
        (when (or (functionp url) (symbolp url))
          (setq url (funcall url)))
        (cl-assert (and (stringp url) (not (string-empty-p url))))
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


;;
(after! evil
  (evil-set-command-property '+lookup/definition :jump t)
  (evil-set-command-property '+lookup/references :jump t)
  (evil-set-command-property '+lookup/documentation :jump t)
  (evil-set-command-property '+lookup/file :jump t))
