;;; lisp/doom-compat.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; This file backports functions and variables from future versions of Emacs (so
;; Emacs 27.x users can enjoy them). Its goal is to support between Emacs 27.x
;; and 30.x.
;;
;;; Code:

;;; From Emacs >= 28
;; `format-spec' wasn't autoloaded until 28.1
(unless (fboundp 'format-spec)
  (autoload 'format-spec "format-spec"))

;; Introduced in 28.1
(unless (fboundp 'ensure-list)
  (defun ensure-list (object)
    "Return OBJECT as a list.
If OBJECT is already a list, return OBJECT itself. If it's not a list, return a
one-element list containing OBJECT."
    (declare (pure t) (side-effect-free t))
    (if (listp object) object (list object))))

;; Introduced in 28.1
(unless (fboundp 'always)
  (defun always (&rest _args)
    "Do nothing and return t.
This function accepts any number of ARGUMENTS, but ignores them.  Also see
`ignore'."
    t))

;; Introduced in 28.1
(unless (fboundp 'file-name-concat)
  (defun file-name-concat (directory &rest components)
    "Append COMPONENTS to DIRECTORY and return the resulting string.

Elements in COMPONENTS must be a string or nil.
DIRECTORY or the non-final elements in COMPONENTS may or may not end
with a slash -- if they don't end with a slash, a slash will be
inserted before contatenating."
    (mapconcat
     #'identity
     (cl-loop for str in (cons directory components)
              if (and str (/= 0 (length str))
                      (if (string-suffix-p "/" str)
                          (substring str 0 -1)
                        str))
              collect it)
     "/")))

;; Introduced in 28.1
(unless (fboundp 'with-environment-variables)
  (defmacro with-environment-variables (variables &rest body)
    "Set VARIABLES in the environment and execute BODY.
VARIABLES is a list of variable settings of the form (VAR VALUE),
where VAR is the name of the variable (a string) and VALUE
is its value (also a string).

The previous values will be restored upon exit."
    (declare (indent 1) (debug (sexp body)))
    (unless (consp variables)
      (error "Invalid VARIABLES: %s" variables))
    `(let ((process-environment (copy-sequence process-environment)))
       ,@(cl-loop for var in variables
                  collect `(setenv ,(car var) ,(cadr var)))
       ,@body)))

;; Introduced in 28.1
(unless (fboundp 'file-name-with-extension)
  (defun file-name-with-extension (filename extension)
    "Return FILENAME modified to have the specified EXTENSION.
The extension (in a file name) is the part that begins with the last \".\".
This function removes any existing extension from FILENAME, and then
appends EXTENSION to it.

EXTENSION may include the leading dot; if it doesn't, this function
will provide it.

It is an error if FILENAME or EXTENSION is empty, or if FILENAME
is in the form of a directory name according to `directory-name-p'.

See also `file-name-sans-extension'."
    (let ((extn (string-trim-left extension "[.]")))
      (cond ((string-empty-p filename)
             (error "Empty filename"))
            ((string-empty-p extn)
             (error "Malformed extension: %s" extension))
            ((directory-name-p filename)
             (error "Filename is a directory: %s" filename))
            ((concat (file-name-sans-extension filename) "." extn))))))


;;; From Emacs >= 29
;; Introduced in Emacs 29.1
(unless (fboundp 'with-memoization)
  (defmacro with-memoization (place &rest code)
    "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
    (declare (indent 1) (debug (gv-place body)))
    (gv-letplace (getter setter) place
      `(or ,getter
           ,(macroexp-let2 nil val (macroexp-progn code)
              `(progn
                 ,(funcall setter val)
                 ,val))))))

;; Introduced in 29.1
(unless (fboundp 'pos-bol) (defalias 'pos-bol #'line-beginning-position))
(unless (fboundp 'pos-eol) (defalias 'pos-eol #'line-end-position))

;; Introduced in 29.1
(unless (boundp 'enable-theme-functions)
  (defcustom enable-theme-functions nil
    "Abnormal hook that is run after a theme has been enabled.
The functions in the hook are called with one parameter -- the
 name of the theme that's been enabled (as a symbol)."
    :type 'hook
    :group 'customize
    :version "29.1")
  (defcustom disable-theme-functions nil
    "Abnormal hook that is run after a theme has been disabled.
The functions in the hook are called with one parameter -- the
 name of the theme that's been disabled (as a symbol)."
    :type 'hook
    :group 'customize
    :version "29.1")
  (define-advice enable-theme (:after (theme) trigger-hooks)
    (run-hook-with-args 'enable-theme-functions theme))
  (define-advice disable-theme (:around (fn theme) trigger-hooks)
    (when (custom-theme-enabled-p theme)
      (funcall fn theme)
      (run-hook-with-args 'enable-theme-functions theme))))


;;; From Emacs >= 30
;; Introduced in 30.1
(unless (fboundp 'major-mode-remap)
  (defvar major-mode-remap-alist nil)  ; introduced in 29.1
  (defvar major-mode-remap-defaults nil)
  (defun major-mode-remap (mode)
    "Return the function to use to enable MODE."
    (or (cdr (or (assq mode major-mode-remap-alist)
                 (assq mode major-mode-remap-defaults)))
        mode))
  (defvar-local set-auto-mode--last nil)
  (define-advice set-auto-mode-0 (:override (mode &optional keep-mode-if-same) backport-major-mode-remap)
    (unless (and keep-mode-if-same
                 (or (eq (indirect-function mode)
                         (indirect-function major-mode))
                     (and set-auto-mode--last
                          (eq mode (car set-auto-mode--last))
                          (eq major-mode (cdr set-auto-mode--last)))))
      (when mode
        (funcall (major-mode-remap mode))
        (unless (eq mode major-mode)
          (setq set-auto-mode--last (cons mode major-mode)))
        mode))))

;; Introduced in 30.1
(unless (boundp 'safe-local-variable-directories)
  (defvar safe-local-variable-directories ())
  (define-advice hack-local-variables-filter
      (:around (fn variables dir-name) backport-safe-local-variable-directories)
    (let ((enable-local-variables
           (if (delq nil (mapcar (lambda (dir)
                                   (and dir-name dir
                                        (file-equal-p dir dir-name)))
                                 safe-local-variable-directories))
               :all
             enable-local-variables)))
      (funcall fn variables dir-name))))

;; Introduced in 30.1
(unless (fboundp 'static-if)
  (defmacro static-if (condition then-form &rest else-forms)
    "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is non-nil,
expand the macro to THEN-FORM.  Otherwise expand it to ELSE-FORMS
enclosed in a `progn' form.  ELSE-FORMS may be empty."
    (declare (indent 2)
             (debug (sexp sexp &rest sexp)))
    (if (eval condition lexical-binding)
        then-form
      (cons 'progn else-forms))))


;;; From Emacs 31+
(unless (fboundp 'static-when)
  (defmacro static-when (condition &rest body)
    "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is non-nil,
expand the macro to evaluate all BODY forms sequentially and return
the value of the last one, or nil if there are none."
    (declare (indent 1) (debug t))
    (if body
        (if (eval condition lexical-binding)
            (cons 'progn body)
          nil)
      (macroexp-warn-and-return (format-message "`static-when' with empty body")
                                (list 'progn nil nil) '(empty-body static-when) t))))

;;; From Emacs 31+
(unless (fboundp 'static-unless)
  (defmacro static-unless (condition &rest body)
    "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is nil,
expand the macro to evaluate all BODY forms sequentially and return
the value of the last one, or nil if there are none."
    (declare (indent 1) (debug t))
    (if body
        (if (eval condition lexical-binding)
            nil
          (cons 'progn body))
      (macroexp-warn-and-return (format-message "`static-unless' with empty body")
                                (list 'progn nil nil) '(empty-body static-unless) t))))

(provide 'doom-compat)
;;; doom-compat.el ends here
