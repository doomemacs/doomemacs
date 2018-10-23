;;; feature/lookup/autoload/lookup.el -*- lexical-binding: t; -*-

(defvar +lookup--last-provider nil)

;;;###autodef
(defun set-lookup-handlers! (modes &rest plist)
  "Define a jump target for major MODES.

This overwrites previously defined handlers for MODES. If used on minor modes,
they are combined with handlers defined for other minor modes or the major mode
it's activated in.

If the CAR of PLIST is nil, other properties are ignored and all existing jump
handlers for MODES are cleared. Otherwise, PLIST accepts the following
properties:

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
  :references along with :xref-backend, those will have higher precedence."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+lookup|init-%s" mode))))
      (cond ((null (car plist))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset fn
                   (lambda ()
                     (when (or (eq major-mode mode)
                               (and (boundp mode)
                                    (symbol-value mode)))
                       (cl-destructuring-bind
                           (&key definition references documentation file xref-backend)
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

;; FIXME obsolete :lookup
;;;###autoload
(def-setting! :lookup (modes &rest plist)
  :obsolete set-lookup-handlers!
  `(set-lookup-handlers! ,modes ,@plist))


;;
;; Library

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
  (cond (initial)
        ((use-region-p)
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
        ((require 'xref nil t)
         (xref-backend-identifier-at-point (xref-find-backend)))))

(defun +lookup--jump-to (prop identifier &optional other-window)
  (cl-loop with origin = (point-marker)
           for fn
           in (plist-get (list :definition +lookup-definition-functions
                               :references +lookup-references-functions
                               :documentation +lookup-documentation-functions
                               :file +lookup-file-functions)
                         prop)
           for cmd = (or (command-remapping fn) fn)
           if (condition-case e
                  (save-window-excursion
                    (when (or (if (commandp cmd)
                                  (call-interactively cmd)
                                (funcall cmd identifier))
                              (/= (point-marker) origin))
                      (point-marker)))
                (error (ignore (message "%s" e))))
           return
           (progn
             (funcall (if other-window
                          #'pop-to-buffer
                        #'pop-to-buffer-same-window)
                      (marker-buffer it))
             (goto-char it))))

(defun +lookup--file-search (identifier)
  (unless identifier
    (let ((query (rxt-quote-pcre identifier)))
      (ignore-errors
        (cond ((featurep! :completion ivy)
               (+ivy-file-search nil :query query)
               t)
              ((featurep! :completion helm)
               (+helm-file-search nil :query query)
               t))))))


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
  (and (featurep 'evil)
       evil-mode
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

        ((and +lookup-definition-functions
              (+lookup--jump-to :definition identifier other-window)))

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

        ((and +lookup-references-functions
              (+lookup--jump-to :references identifier other-window)))

        ((error "Couldn't find references of '%s'" identifier))))

;;;###autoload
(defun +lookup/documentation (identifier &optional other-window)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

Goes down a list of possible backends:

1. The :documentation spec defined with by `set-lookup-handlers!'
2. If the +docsets flag is active for :feature lookup, use `+lookup/in-docsets'
3. Fall back to an online search, with `+lookup/online'"
  (interactive
   (list (+lookup--symbol-or-region)
         current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))

        ((and +lookup-documentation-functions
              (+lookup--jump-to :documentation identifier other-window)))

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

        ((not (and +lookup-file-functions
                   (+lookup--jump-to :file path)))
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
