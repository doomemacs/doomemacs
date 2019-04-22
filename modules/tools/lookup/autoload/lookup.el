;;; tools/lookup/autoload/lookup.el -*- lexical-binding: t; -*-

(defvar +lookup--handler-alist nil)

;;;###autodef
(cl-defun set-lookup-handlers!
    (modes &rest plist &key definition references documentation file xref-backend async)
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
  the async response and return 'fallback."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+lookup|init-%s" mode))))
      (cond ((null (car plist))
             (remove-hook hook fn)
             (delq! mode +lookup--handler-alist 'assq)
             (unintern fn nil))
            ((fset fn
                   (lambda ()
                     (when (or (eq major-mode mode)
                               (and (boundp mode)
                                    (symbol-value mode)))
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
                                      'xref-backend-functions)))))
             (add-hook hook fn))))))


;;
;;; Helpers

(defun +lookup--set-handler (spec functions-var &optional async)
  (when spec
    (cl-destructuring-bind (fn . plist)
        (doom-enlist spec)
      (put fn '+lookup-plist (plist-put plist :async async))
      (add-hook functions-var fn nil t))))

(defun +lookup--symbol-or-region (&optional initial)
  (cond ((stringp initial)
         initial)
        ((use-region-p)
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
        ((require 'xref nil t)
         (xref-backend-identifier-at-point (xref-find-backend)))))

(defun +lookup--run-handler (handler identifier)
  (if (commandp handler)
      (call-interactively handler)
    (funcall handler identifier)))

(defun +lookup--run-handlers (handler identifier origin &optional other-window)
  (doom-log "Looking up '%s' with '%s'" identifier handler)
  (condition-case e
      (let ((plist (get handler '+lookup-plist)))
        (cond ((plist-get plist :async)
               (when other-window
                 ;; If async, we can't catch the window change or destination
                 ;; buffer reliably, so we set up the new window ahead of time.
                 (switch-to-buffer-other-window (current-buffer))
                 (goto-char (marker-position origin)))
               (+lookup--run-handler handler identifier)
               t)
              ((save-window-excursion
                 (and (or (+lookup--run-handler handler identifier)
                          (null origin)
                          (/= (point-marker) origin))
                      (point-marker))))))
    ((error user-error debug)
     (message "Lookup handler %S: %s" handler e)
     nil)))

(defun +lookup--jump-to (prop identifier &optional other-window)
  (let ((result
         (run-hook-wrapped
          (plist-get (list :definition '+lookup-definition-functions
                           :references '+lookup-references-functions
                           :documentation '+lookup-documentation-functions
                           :file '+lookup-file-functions)
                     prop)
          #'+lookup--run-handlers
          identifier
          (point-marker)
          other-window)))
    (if (not (markerp result))
        (ignore (message "No lookup handler could find %S" identifier))
      (funcall (if other-window
                   #'switch-to-buffer-other-window
                 #'switch-to-buffer)
               (marker-buffer result))
      (goto-char result)
      (better-jumper-set-jump)
      result)))


;;
;;; Lookup backends

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

(defun +lookup-evil-goto-definition-backend (_identifier)
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


;;
;;; Main commands

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
(defun +lookup/documentation (identifier &optional _arg)
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
;;; Source-specific commands

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
