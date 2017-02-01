;;; core-lib.el

(defmacro λ! (&rest body)
  "A shortcut for inline keybind lambdas."
  `(lambda () (interactive) ,@body))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load', that supresses warnings
during compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.

HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.

FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms will be
wrapped in a lambda. A list of symbols will expand into a series of add-hook calls.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))"
  (declare (indent defun) (debug t))
  (unless func-or-forms
    (error "add-hook!: FUNC-OR-FORMS is empty"))
  (let* ((val (car func-or-forms))
         (quoted-p (eq (car-safe hook) 'quote))
         (hook (if quoted-p (cadr hook) hook))
         (funcs (if (eq (car-safe val) 'quote)
                    (if (cdr-safe (cadr val))
                        (cadr val)
                      (list (cadr val)))
                  (list func-or-forms))))
    (macroexp-progn
     (mapcar (lambda (f)
               (let ((func (if (symbolp f) `(quote ,f) `(lambda (&rest _) ,@func-or-forms))))
                 (macroexp-progn
                  (mapcar (lambda (h)
                            `(add-hook ',(if quoted-p h (intern (format "%s-hook" h))) ,func))
                          (-list hook)))))
             funcs))))

(defmacro associate! (mode &rest rest)
  "Associate a major or minor mode to certain patterns and project files."
  (declare (indent 1))
  (let ((minor (plist-get rest :minor))
        (in    (plist-get rest :in))
        (match (plist-get rest :match))
        (files (plist-get rest :files))
        (pred  (plist-get rest :when)))
    (cond ((or files in pred)
           (when (and files (not (or (listp files) (stringp files))))
             (user-error "associate! :files expects a string or list of strings"))
           (let ((hook-name (intern (format "doom--init-mode-%s" mode))))
             (macroexp-progn
              (list `(defun ,hook-name ()
                       (when (and ,(if match `(if buffer-file-name (string-match-p ,match buffer-file-name)) t)
                                  (or ,(not files)
                                      (and (boundp ',mode)
                                           (not ,mode)
                                           (doom-project-has-files ,@(-list files))))
                                  (or (not ,pred)
                                      (funcall ,pred buffer-file-name)))
                         (,mode 1)))
                    (if (and in (listp in))
                        (macroexp-progn
                         (mapcar (lambda (h) `(add-hook ',h ',hook-name))
                                 (mapcar (lambda (m) (intern (format "%s-hook" m))) in)))
                      `(add-hook 'find-file-hook ',hook-name))))))
          (match
           `(add-to-list ',(if minor 'doom-auto-minor-mode-alist 'auto-mode-alist)
                         (cons ,match ',mode)))
          (t (user-error "associate! invalid rules for mode [%s] (in %s) (match %s) (files %s)"
                         mode in match files)))))

(provide 'core-lib)
;;; core-lib.el ends here
