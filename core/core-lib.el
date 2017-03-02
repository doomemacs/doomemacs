;;; core-lib.el

(require 'cl-lib)
(eval-when-compile
  (require 'subr-x))

;; I don't use use-package for these to save on the `fboundp' lookups it does
;; for its :commands property.
(mapc (lambda (sym) (autoload sym "async"))
      '(async-start async-start-process async-byte-recompile-directory))

(mapc (lambda (sym) (autoload sym "persistent-soft"))
      '(persistent-soft-exists-p persistent-soft-fetch persistent-soft-flush persistent-soft-store))

(mapc (lambda (sym) (autoload sym "s"))
      '(s-trim s-trim-left s-trim-right s-chomp s-collapse-whitespace s-word-wrap
        s-center s-pad-left s-pad-right s-truncate s-left s-right s-chop-suffix
        s-chop-suffixes s-chop-prefix s-chop-prefixes s-shared-start s-shared-end
        s-repeat s-concat s-prepend s-append s-lines s-match s-match-strings-all
        s-matched-positions-all s-slice-at s-split s-split-up-to s-join s-equals?
        s-less? s-matches? s-blank? s-present? s-ends-with? s-starts-with? s-contains?
        s-lowercase? s-uppercase? s-mixedcase? s-capitalized? s-numeric? s-replace
        s-replace-all s-downcase s-upcase s-capitalize s-titleize s-with s-index-of
        s-reverse s-presence s-format s-lex-format s-count-matches s-wrap s-split-words
        s-lower-camel-case s-upper-camel-case s-snake-case s-dashed-words
        s-capitalized-words s-titleized-words s-word-initials))


;;
;; Helpers
;;

(defun doom--resolve-paths (paths &optional root)
  (cond ((stringp paths)
         `(file-exists-p
           (expand-file-name
            ,paths ,(if (or (string-prefix-p "./" paths)
                            (string-prefix-p "../" paths))
                        'default-directory
                      root))))
        ((listp paths)
         (let (forms)
           (dolist (i paths (nreverse forms))
             (push (doom--resolve-paths i root) forms))))
        (t paths)))

(defun doom--resolve-hooks (hooks)
  (let ((quoted-p (eq (car-safe hooks) 'quote))
        ret-hooks)
    (when quoted-p
      (setq hooks (cadr hooks)))
    (dolist (hook (if (listp hooks) hooks (list hooks)) (nreverse ret-hooks))
      (push (cond ((eq (car-safe hook) 'quote)
                   (cadr hook))
                  ((string-suffix-p "-hook" (symbol-name hook))
                   hook)
                  (t
                   (intern (format "%s-hook" (symbol-name hook)))))
            ret-hooks))))


;;
;; Library
;;

(defmacro λ! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if doom-debug-mode
       (progn ,@forms)
     (fset 'doom--old-write-region-fn (symbol-function 'write-region))
     (cl-letf ((standard-output (lambda (&rest _)))
               ((symbol-function 'load-file) (lambda (file) (load file nil t)))
               ((symbol-function 'message) (lambda (&rest _)))
               ((symbol-function 'write-region)
                (lambda (start end filename &optional append visit lockname mustbenew)
                  (unless visit (setq visit 'no-message))
                  (doom--old-write-region-fn
                   start end filename append visit lockname mustbenew)))
               (inhibit-message t)
               (save-silently t))
       ,@forms)))

(defmacro add-transient-hook! (hook &rest forms)
  "Attaches transient forms to a hook (can also be a function, which will be
advised instead). These forms will be evaluated only once when that
function/hook is first invoked, then it detaches itself."
  (declare (indent 1))
  (let ((fn (intern (format "doom--transient-hook-%s" hook))))
    `(progn
       (defun ,fn (&rest _)
         ,@forms
         ,(cond ((functionp hook) `(advice-remove ',hook ',fn))
                ((symbolp hook) `(remove-hook ',hook ',fn))))
       ,(cond ((functionp hook) `(advice-add ',hook :before ',fn))
              ((symbolp hook) `(add-hook ',hook ',fn))))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let (hook append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))))
    (let ((hooks (doom--resolve-hooks (pop args)))
          (funcs
           (let ((val (car args)))
             (if (eq (car-safe val) 'quote)
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(quote ,fn)
                   `(lambda (&rest args) ,@args)))
        (dolist (hook hooks)
          (push `(,(if (boundp 'hook-fn) hook-fn 'add-hook)
                  ',hook ,fn ,append-p ,local-p)
                forms)))
      `(progn ,@(nreverse forms)))))

(defmacro remove-hook! (&rest args)
  "Convenience macro for `remove-hook'. Takes the same arguments as
`add-hook!'."
  (let ((hook-fn 'remove-hook))
    (macroexpand `(add-hook! ,@args))))

(defmacro associate! (mode &rest plist)
  "Associate a minor mode to certain patterns and project files."
  (declare (indent 1))
  (unless noninteractive
    (let* ((modes (plist-get plist :modes))
           (match (plist-get plist :match))
           (files (plist-get plist :files))
           (pred-form (plist-get plist :when)))
      (cond ((or files modes pred-form)
             (when (and files
                        (not (or (listp files)
                                 (stringp files))))
               (user-error "associate! :files expects a string or list of strings"))
             (let ((hook-name (intern (format "doom--init-mode-%s" mode))))
               `(progn
                  (defun ,hook-name ()
                    (when (and (boundp ',mode)
                               (not ,mode)
                               ,(if match `(if buffer-file-name (string-match-p ,match buffer-file-name)) t)
                               ,(if files (doom--resolve-paths files) t)
                               ,(or pred-form t))
                      (,mode 1)))
                  ,@(if (and modes (listp modes))
                        (let (forms)
                          (dolist (hook (doom--resolve-hooks modes) (nreverse forms))
                            (push `(add-hook ',hook ',hook-name) forms)))
                      `((add-hook 'after-change-major-mode-hook ',hook-name))))))
            (match
             `(push (cons ,match ',mode) doom-auto-minor-mode-alist))
            (t (user-error "associate! invalid rules for mode [%s] (modes %s) (match %s) (files %s)"
                           mode modes match files))))))


;; Provides a centralized configuration system that a) won't evaluate its
;; arguments if it doesn't need to (performance), b) won't complain if the
;; setting doesn't exist and c) is more elegant than a bunch of `after!' blocks,
;; which can cause intermittent stuttering in large quantities. I'm a fan of
;; concise, do-what-I-mean front-facing configuration, believe it or not.
;;
;; Plus, it can benefit from byte-compilation.

(defmacro def-setting! (keyword arglist &optional docstring &rest forms)
  "Define a setting macro. Like `defmacro', this should return a form to be
executed when called with `set!'. FORMS are not evaluated until `set!' calls it."
  (declare (indent defun) (doc-string 3))
  (unless (keywordp keyword)
    (error "Not a valid property name: %s" keyword))
  `(defun ,(intern (format "doom-setting--setter%s" keyword)) ,arglist
     ,docstring
     ,@forms))

(defmacro set! (keyword &rest values)
  "Set an option defined by `def-setting!'. Skip if doesn't exist."
  (declare (indent defun))
  (unless values
    (error "Empty set! for %s" keyword))
  (let ((fn (intern (format "doom-setting--setter%s" keyword))))
    (if (functionp fn)
        (apply fn (eval `(list ,@values)))
      (when doom-debug-mode
        (message "No setting found for %s" keyword)))))

(provide 'core-lib)
;;; core-lib.el ends here
