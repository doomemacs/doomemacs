;;; tools/lookup/autoload/lookup.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-lookup-handlers! (modes &rest plist)
  "Define jump handlers for major or minor MODES.

A handler is either an interactive command that changes the current buffer
and/or location of the cursor, or a function that takes one argument: the
identifier being looked up, and returns either nil (failed to find it), t
(succeeded at changing the buffer/moving the cursor), or 'deferred (assume this
handler has succeeded, but expect changes not to be visible yet).

There are several kinds of handlers, which can be defined with the following
properties:

:definition FN
  Run when jumping to a symbol's definition. Used by `+lookup/definition'.
:references FN
  Run when looking for usage references of a symbol in the current project. Used
  by `+lookup/references'.
:documentation FN
  Run when looking up documentation for a symbol. Used by
  `+lookup/documentation'.
:file FN
  Run when looking up the file for a symbol/string. Typically a file path. Used
  by `+lookup/file'.
:xref-backend FN
  Defines an xref backend for a major-mode. A :definition and :references
  handler isn't necessary with a :xref-backend, but will have higher precedence
  if they exist.
:async BOOL
  Indicates that *all* supplied FNs are asynchronous. Note: lookups will not try
  any handlers after async ones, due to their nature. To get around this, you
  must write a specialized wrapper to await the async response, or use a
  different heuristic to determine, ahead of time, whether the async call will
  succeed or not.

  If you only want to specify one FN is async, declare it inline instead:

    (set-lookup-handlers! 'rust-mode
      :definition '(racer-find-definition :async t))

Handlers can either be interactive or non-interactive. Non-interactive handlers
must take one argument: the identifier being looked up. This function must
change the current buffer or window or return non-nil when it succeeds.

If it doesn't change the current buffer, or it returns nil, the lookup module
will fall back to the next handler in `+lookup-definition-functions',
`+lookup-references-functions', `+lookup-file-functions' or
`+lookup-documentation-functions'.

Consecutive `set-lookup-handlers!' calls will overwrite previously defined
handlers for MODES. If used on minor modes, they are stacked onto handlers
defined for other minor modes or the major mode it's activated in.

This can be passed nil as its second argument to unset handlers for MODES. e.g.

  (set-lookup-handlers! 'python-mode nil)

\(fn MODES &key DEFINITION REFERENCES DOCUMENTATION FILE XREF-BACKEND ASYNC)"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+lookup--init-%s-handlers-h" mode))))
      (cond ((null (car plist))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset
              fn
              (lambda ()
                (cl-destructuring-bind (&key definition references documentation file xref-backend async)
                    plist
                  (cl-mapc #'+lookup--set-handler
                           (list definition
                                 references
                                 documentation
                                 file
                                 xref-backend)
                           (list '+lookup-definition-functions
                                 '+lookup-references-functions
                                 '+lookup-documentation-functions
                                 '+lookup-file-functions
                                 'xref-backend-functions)
                           (make-list 5 async)
                           (make-list 5 (or (eq major-mode mode)
                                            (and (boundp mode)
                                                 (symbol-value mode))))))))
             (add-hook hook fn))))))


;;
;;; Helpers

(defun +lookup--set-handler (spec functions-var &optional async enable)
  (when spec
    (cl-destructuring-bind (fn . plist)
        (doom-enlist spec)
      (if (not enable)
          (remove-hook functions-var fn 'local)
        (put fn '+lookup-async (or (plist-get plist :async) async))
        (add-hook functions-var fn nil 'local)))))

(defun +lookup--run-handler (handler identifier)
  (if (commandp handler)
      (call-interactively handler)
    (funcall handler identifier)))

(defun +lookup--run-handlers (handler identifier origin)
  (doom-log "Looking up '%s' with '%s'" identifier handler)
  (condition-case-unless-debug e
      (let ((wconf (current-window-configuration))
            (result (condition-case-unless-debug e
                        (+lookup--run-handler handler identifier)
                      (error
                       (doom-log "Lookup handler %S threw an error: %s" handler e)
                       'fail))))
        (cond ((eq result 'fail)
               (set-window-configuration wconf)
               nil)
              ((or (get handler '+lookup-async)
                   (eq result 'deferred)))
              ((or result
                   (null origin)
                   (/= (point-marker) origin))
               (prog1 (point-marker)
                 (set-window-configuration wconf)))))
    ((error user-error)
     (message "Lookup handler %S: %s" handler e)
     nil)))

(defun +lookup--jump-to (prop identifier &optional display-fn arg)
  (let* ((origin (point-marker))
         (handlers
          (plist-get (list :definition '+lookup-definition-functions
                           :references '+lookup-references-functions
                           :documentation '+lookup-documentation-functions
                           :file '+lookup-file-functions)
                     prop))
         (result
          (if arg
              (if-let
                  (handler
                   (intern-soft
                    (completing-read "Select lookup handler: "
                                     (delete-dups
                                      (remq t (append (symbol-value handlers)
                                                      (default-value handlers))))
                                     nil t)))
                  (+lookup--run-handlers handler identifier origin)
                (user-error "No lookup handler selected"))
            (run-hook-wrapped handlers #'+lookup--run-handlers identifier origin))))
    (when (cond ((null result)
                 (message "No lookup handler could find %S" identifier)
                 nil)
                ((markerp result)
                 (funcall (or display-fn #'switch-to-buffer)
                          (marker-buffer result))
                 (goto-char result)
                 result)
                (result))
      (with-current-buffer (marker-buffer origin)
        (better-jumper-set-jump (marker-position origin)))
      result)))

;;;###autoload
(defun +lookup-symbol-or-region (&optional initial)
  "Grab the symbol at point or selected region."
  (cond ((stringp initial)
         initial)
        ((use-region-p)
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
        ((require 'xref nil t)
         ;; A little smarter than using `symbol-at-point', though in most cases,
         ;; xref ends up using `symbol-at-point' anyway.
         (xref-backend-identifier-at-point (xref-find-backend)))))


;;
;;; Lookup backends

(defun +lookup--xref-show (fn identifier)
  (let ((xrefs (funcall fn
                        (xref-find-backend)
                        identifier)))
    (when xrefs
      (xref--show-xrefs xrefs nil)
      (if (cdr xrefs)
          'deferred
        t))))

(defun +lookup-xref-definitions-backend-fn (identifier)
  "Non-interactive wrapper for `xref-find-definitions'"
  (+lookup--xref-show 'xref-backend-definitions identifier))

(defun +lookup-xref-references-backend-fn (identifier)
  "Non-interactive wrapper for `xref-find-references'"
  (+lookup--xref-show 'xref-backend-references identifier))

(defun +lookup-dumb-jump-backend-fn (_identifier)
  "Look up the symbol at point (or selection) with `dumb-jump', which conducts a
project search with ag, rg, pt, or git-grep, combined with extra heuristics to
reduce false positives.

This backend prefers \"just working\" over accuracy."
  (and (require 'dumb-jump nil t)
       (dumb-jump-go)))

(defun +lookup-project-search-backend-fn (identifier)
  "Conducts a simple project text search for IDENTIFIER.

Uses and requires `+ivy-file-search' or `+helm-file-search'. Will return nil if
neither is available. These require ripgrep to be installed."
  (unless identifier
    (let ((query (rxt-quote-pcre identifier)))
      (ignore-errors
        (cond ((featurep! :completion ivy)
               (+ivy-file-search :query query)
               t)
              ((featurep! :completion helm)
               (+helm-file-search :query query)
               t))))))

(defun +lookup-evil-goto-definition-backend-fn (_identifier)
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


;;
;;; Main commands

;;;###autoload
(defun +lookup/definition (identifier &optional arg)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive (list (+lookup-symbol-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :definition identifier nil arg))
        ((error "Couldn't find the definition of %S" identifier))))

;;;###autoload
(defun +lookup/references (identifier &optional arg)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)

Tries each function in `+lookup-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive (list (+lookup-symbol-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :references identifier nil arg))
        ((error "Couldn't find references of %S" identifier))))

;;;###autoload
(defun +lookup/documentation (identifier &optional arg)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

First attempts the :documentation handler specified with `set-lookup-handlers!'
for the current mode/buffer (if any), then falls back to the backends in
`+lookup-documentation-functions'."
  (interactive (list (+lookup-symbol-or-region)
                     current-prefix-arg))
  (cond ((+lookup--jump-to :documentation identifier #'pop-to-buffer arg))
        ((user-error "Couldn't find documentation for %S" identifier))))

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
           (+lookup-symbol-or-region))))))
  (require 'ffap)
  (cond ((not path)
         (call-interactively #'find-file-at-point))

        ((ffap-url-p path)
         (find-file-at-point path))

        ((not (+lookup--jump-to :file path))
         (let ((fullpath (doom-path path)))
           (when (and buffer-file-name (file-equal-p fullpath buffer-file-name))
             (user-error "Already here"))
           (let* ((insert-default-directory t)
                  (project-root (doom-project-root))
                  (ffap-file-finder
                   (cond ((not (doom-glob fullpath))
                          #'find-file)
                         ((ignore-errors (file-in-directory-p fullpath project-root))
                          (lambda (dir)
                            (let* ((default-directory dir)
                                   projectile-project-name
                                   projectile-project-root
                                   (projectile-project-root-cache (make-hash-table :test 'equal))
                                   (file (projectile-completing-read "Find file: "
                                                                     (projectile-current-project-files)
                                                                     :initial-input path)))
                              (find-file (expand-file-name file (doom-project-root)))
                              (run-hooks 'projectile-find-file-hook))))
                         (#'doom-project-browse))))
             (find-file-at-point path))))))


;;
;;; Dictionary

;;;###autoload
(defun +lookup/word-definition (identifier &optional arg)
  "Look up the definition of the word at point (or selection)."
  (interactive
   (list (+lookup-symbol-or-region)
         current-prefix-arg))
  (unless (featurep! +dictionary)
    (user-error "The +dictionary feature hasn't be enabled on :tools lookup module"))
  (cond (IS-MAC
         (osx-dictionary-search-input identifier))
        (+lookup-dictionary-enable-online
         (define-word identifier nil arg))
        ;; TODO Implement offline dictionary backend
        ((user-error "No offline dictionary defined yet"))))

;;;###autoload
(defun +lookup/word-synonyms (identifier &optional arg)
  "Look up and insert a synonym for the word at point (or selection)."
  (interactive
   (list (+lookup-symbol-or-region)
         current-prefix-arg))
  (unless (featurep! +dictionary)
    (user-error "The +dictionary feature hasn't be enabled on :tools lookup module"))
  (if +lookup-dictionary-enable-online
      (request
        (powerthesaurus-compose-url identifier)
        :parser (lambda () (libxml-parse-html-region (point) (point-max)))
        :headers '(("User-Agent" . "Chrome/74.0.3729.169"))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    ;; in order to allow users to quit powerthesaurus prompt
                    ;; with C-g, we need to wrap callback with this
                    (with-local-quit
                      (funcall (powerthesaurus-choose-callback
                                (region-beginning) (region-end))
                               (powerthesaurus-pick-synonym data)
                               identifier)))))
    ;; TODO Implement offline synonyms backend
    (user-error "No offline dictionary implemented yet")))
