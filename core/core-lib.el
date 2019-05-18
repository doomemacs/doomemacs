;;; core-lib.el -*- lexical-binding: t; -*-

;;
;;; Helpers

(defun doom--resolve-path-forms (spec &optional directory)
  "Converts a simple nested series of or/and forms into a series of
`file-exists-p' checks.

For example

  (doom--resolve-path-forms
    '(or A (and B C))
    \"~\")

Returns (approximately):

  '(let* ((_directory \"~\")
          (A (expand-file-name A _directory))
          (B (expand-file-name B _directory))
          (C (expand-file-name C _directory)))
     (or (and (file-exists-p A) A)
         (and (if (file-exists-p B) B)
              (if (file-exists-p C) C))))

This is used by `associate!', `file-exists-p!' and `project-file-exists-p!'."
  (declare (pure t) (side-effect-free t))
  (cond ((stringp spec)
         `(let ((--file-- ,(if (file-name-absolute-p spec)
                             spec
                           `(expand-file-name ,spec ,directory))))
            (and (file-exists-p --file--)
                 --file--)))
        ((and (listp spec)
              (memq (car spec) '(or and)))
         `(,(car spec)
           ,@(cl-loop for i in (cdr spec)
                      collect (doom--resolve-path-forms i directory))))
        ((or (symbolp spec)
             (listp spec))
         `(let ((--file-- ,(if (and directory
                                    (or (not (stringp directory))
                                        (file-name-absolute-p directory)))
                               `(expand-file-name ,spec ,directory)
                             spec)))
            (and (file-exists-p --file--)
                 --file--)))
        (spec)))

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-enlist (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun doom--assert-stage-p (stage macro)
  (unless (bound-and-true-p byte-compile-current-file)
    (cl-assert (eq stage doom--stage)
               nil
               "Found %s call in non-%s.el file (%s)"
               macro (symbol-name stage)
               (let ((path (FILE!)))
                 (if (file-in-directory-p path doom-emacs-dir)
                     (file-relative-name path doom-emacs-dir)
                   (abbreviate-file-name path))))))


;;
;;; Public library

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun doom-keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun doom-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type :test keyword)
  (substring (symbol-name keyword) 1))

(defmacro doom-log (format-string &rest args)
  "Log to *Messages* if `doom-debug-mode' is on.
Does not interrupt the minibuffer if it is in use, but still logs to *Messages*.
Accepts the same arguments as `message'."
  `(when doom-debug-mode
     (let ((inhibit-message (active-minibuffer-window)))
       (message
        ,(concat (propertize "DOOM " 'face 'font-lock-comment-face)
                 (when doom--current-module
                   (propertize
                    (format "[%s/%s] "
                            (doom-keyword-name (car doom--current-module))
                            (cdr doom--current-module))
                    'face 'warning))
                 format-string)
        ,@args))))

(defun FILE! ()
  "Return the emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
        (load-file-name)
        (buffer-file-name)
        ((stringp (car-safe current-load-list)) (car current-load-list))))

(defun DIR! ()
  "Returns the directory of the emacs lisp file this macro is called from."
  (let ((file (FILE!)))
    (and file (file-name-directory file))))


;;
;; Macros

(defmacro λ! (&rest body)
  "Expands to (lambda () (interactive) ,@body)."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defalias 'lambda! 'λ!)

(defmacro pushnew! (place &rest values)
  "Like `cl-pushnew', but will prepend VALUES to PLACE.
The order VALUES is preserved."
  `(dolist (--value-- (nreverse (list ,@values)))
     (cl-pushnew --value-- ,place)))

(defmacro delq! (elt list &optional fetcher)
  "Delete ELT from LIST in-place."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defmacro defer-until! (condition &rest body)
  "Run BODY when CONDITION is true (checks on `after-load-functions'). Meant to
serve as a predicated alternative to `after!'."
  (declare (indent defun) (debug t))
  `(if ,condition
       (progn ,@body)
     ,(let ((fun (make-symbol "doom|delay-form-")))
        `(progn
           (fset ',fun (lambda (&rest args)
                         (when ,(or condition t)
                           (remove-hook 'after-load-functions #',fun)
                           (unintern ',fun nil)
                           (ignore args)
                           ,@body)))
           (put ',fun 'permanent-local-hook t)
           (add-hook 'after-load-functions #',fun)))))

(defmacro defer-feature! (feature &optional mode)
  "Pretend FEATURE hasn't been loaded yet, until FEATURE-hook is triggered.

Some packages (like `elisp-mode' and `lisp-mode') are loaded immediately at
startup, which will prematurely trigger `after!' (and `with-eval-after-load')
blocks. To get around this we make Emacs believe FEATURE hasn't been loaded yet,
then wait until FEATURE-hook (or MODE-hook, if MODE is provided) is triggered to
reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "doom|defer-feature-%s" feature)))
        (mode (or mode feature)))
    `(progn
       (setq features (delq ',feature features))
       (advice-add #',mode :before #',advice-fn)
       (defun ,advice-fn (&rest _)
         ;; Some plugins (like yasnippet) will invoke a mode early to parse
         ;; code, which would prematurely trigger this. In those cases, well
         ;; behaved plugins will use `delay-mode-hooks', which we can check for:
         (when (and ,(intern (format "%s-hook" mode))
                    (not delay-mode-hooks))
           ;; ...Otherwise, announce to the world this package has been loaded,
           ;; so `after!' handlers can react.
           (provide ',feature)
           (advice-remove #',mode #',advice-fn))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load-file', `write-region' and anything that
writes to `standard-output'."
  `(cond (noninteractive
          (let ((old-fn (symbol-function 'write-region)))
            (cl-letf ((standard-output (lambda (&rest _)))
                      ((symbol-function 'load-file) (lambda (file) (load file nil t)))
                      ((symbol-function 'message) (lambda (&rest _)))
                      ((symbol-function 'write-region)
                       (lambda (start end filename &optional append visit lockname mustbenew)
                         (unless visit (setq visit 'no-message))
                         (funcall old-fn start end filename append visit lockname mustbenew))))
              ,@forms)))
         ((or doom-debug-mode debug-on-error debug-on-quit)
          ,@forms)
         ((let ((inhibit-message t)
                (save-silently t))
            (prog1 ,@forms (message ""))))))

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        (fn (if (symbolp (car forms))
                (intern (format "doom|transient-hook-%s" (pop forms)))
              (make-symbol "doom|transient-hook"))))
    `(let ((sym ,hook-or-function))
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (let ((sym ,hook-or-function))
                 (cond ((functionp sym) (advice-remove sym #',fn))
                       ((symbolp sym)   (remove-hook sym #',fn))))
               (unintern ',fn nil)))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(defmacro add-hook! (&rest args)
  "A convenience macro for adding N functions to M hooks.

If N and M = 1, there's no benefit to using this macro over `add-hook'.

This macro accepts, in order:

  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hook(s) to be added to: either an unquoted mode, an unquoted list of
     modes, a quoted hook variable or a quoted list of hook variables. If
     unquoted, '-hook' will be appended to each symbol.
  3. The function(s) to be added: this can be one function, a list thereof, or
     body forms (implicitly wrapped in a closure).

Examples:
    (add-hook! 'some-mode-hook 'enable-something)   (same as `add-hook')
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

\(fn [:append :local] HOOKS FUNCTIONS)"
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (doom--resolve-hook-forms (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (if (eq hook-fn 'remove-hook)
                    `(remove-hook ',hook ,fn ,local-p)
                  `(add-hook ',hook ,fn ,append-p ,local-p))
                forms)))
      `(progn ,@(if append-p (nreverse forms) forms)))))

(defmacro remove-hook! (&rest args)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn [:append :local] HOOKS FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! :remove ,@args))

(defmacro setq-hook! (hooks &rest rest)
  "Sets buffer-local variables on HOOKS.

  (setq-hook! 'markdown-mode-hook
    line-spacing 2
    fill-column 80)

\(fn HOOKS &rest SYM VAL...)"
  (declare (indent 1))
  (unless (= 0 (% (length rest) 2))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (let* ((vars (let ((args rest)
                     vars)
                 (while args
                   (push (symbol-name (car args)) vars)
                   (setq args (cddr args)))
                 vars))
         (fnsym (intern (format "doom|setq-%s" (string-join (sort vars #'string-lessp) "-")))))
    (macroexp-progn
     (append `((fset ',fnsym
                     (lambda (&rest _)
                       ,@(let (forms)
                           (while rest
                             (let ((var (pop rest))
                                   (val (pop rest)))
                               (push `(set (make-local-variable ',var) ,val) forms)))
                           (nreverse forms)))))
             (cl-loop for hook in (doom--resolve-hook-forms hooks)
                      collect `(add-hook ',hook #',fnsym 'append))))))

(defun advice-add! (symbols where functions)
  "Variadic version of `advice-add'.

SYMBOLS and FUNCTIONS can be lists of functions."
  (let ((functions (if (functionp functions)
                       (list functions)
                     functions)))
    (dolist (s (doom-enlist symbols))
      (dolist (f (doom-enlist functions))
        (advice-add s where f)))))

(defun advice-remove! (symbols where-or-fns &optional functions)
  "Variadic version of `advice-remove'.

WHERE-OR-FNS is ignored if FUNCTIONS is provided. This lets you substitute
advice-add with advice-remove and evaluate them without having to modify every
statement."
  (unless functions
    (setq functions where-or-fns
          where-or-fns nil))
  (let ((functions (if (functionp functions)
                       (list functions)
                     functions)))
    (dolist (s (doom-enlist symbols))
      (dolist (f (doom-enlist functions))
        (advice-remove s f)))))

(cl-defmacro associate! (mode &key modes match files when)
  "Enables a minor mode if certain conditions are met.

The available conditions are:

  :modes SYMBOL_LIST
    A list of major/minor modes in which this minor mode may apply.
  :match REGEXP
    A regexp to be tested against the current file path.
  :files SPEC
    Accepts what `project-file-exists-p!' accepts. Checks if certain files or
    directories exist relative to the project root.
  :when FORM
    Whenever FORM returns non-nil."
  (declare (indent 1))
  (unless noninteractive
    (cond ((or files modes when)
           (when (and files
                      (not (or (listp files)
                               (stringp files))))
             (user-error "associate! :files expects a string or list of strings"))
           (let ((hook-name (intern (format "doom--init-mode-%s" mode))))
             `(progn
                (fset ',hook-name
                      (lambda ()
                        (and (fboundp ',mode)
                             (not (bound-and-true-p ,mode))
                             (and buffer-file-name (not (file-remote-p buffer-file-name)))
                             ,(or (not match)
                                  `(if buffer-file-name (string-match-p ,match buffer-file-name)))
                             ,(or (not files)
                                  (doom--resolve-path-forms
                                   (if (stringp (car files)) (cons 'and files) files)
                                   '(doom-project-root)))
                             ,(or when t)
                             (,mode 1))))
                ,@(if (and modes (listp modes))
                      (cl-loop for hook in (doom--resolve-hook-forms modes)
                               collect `(add-hook ',hook #',hook-name))
                    `((add-hook 'after-change-major-mode-hook #',hook-name))))))
          (match
           `(add-to-list 'doom-auto-minor-mode-alist '(,match . ,mode)))
          ((user-error "Invalid `associate!' rules for mode [%s] (:modes %s :match %s :files %s :when %s)"
                       mode modes match files when)))))

(defmacro file-exists-p! (spec &optional directory)
  "Returns non-nil if the files in SPEC all exist.

Returns the last file found to meet the rules set by SPEC. SPEC can be a single
file or a list of forms/files. It understands nested (and ...) and (or ...), as
well.

DIRECTORY is where to look for the files in SPEC if they aren't absolute.

For example:
  (file-exists-p! (or doom-core-dir \"~/.config\" \"some-file\") \"~\")"
  (if directory
      `(let ((--directory-- ,directory))
         ,(doom--resolve-path-forms spec '--directory--))
    (doom--resolve-path-forms spec)))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (unless path
    (setq path (or (DIR!)
                   (error "Could not detect path to look for '%s' in"
                          filename))))
  (let ((file (if path `(expand-file-name ,filename ,path) filename)))
    `(condition-case e
         (load ,file ,noerror ,(not doom-debug-mode))
       ((debug doom-error) (signal (car e) (cdr e)))
       ((debug error)
        (let* ((source (file-name-sans-extension ,file))
               (err (cond ((file-in-directory-p source doom-core-dir)
                           (cons 'doom-error doom-core-dir))
                          ((file-in-directory-p source doom-private-dir)
                           (cons 'doom-private-error doom-private-dir))
                          ((cons 'doom-module-error doom-emacs-dir)))))
          (signal (car err)
                  (list (file-relative-name
                         (concat source ".el")
                         (cdr err))
                        e)))))))

(defmacro custom-set-faces! (&rest spec-groups)
  "Convenience macro for additively setting face attributes.

SPEC-GROUPS is a list of either face specs, or alists mapping a package name to
a list of face specs. e.g.

  (custom-set-faces!
   (mode-line :foreground (doom-color 'blue))
   (mode-line-buffer-id :foreground (doom-color 'fg) :background \"#000000\")
   (mode-line-success-highlight :background (doom-color 'green))
   (org
    (org-tag :background \"#4499FF\")
    (org-ellipsis :inherit 'org-tag))
   (which-key
    (which-key-docstring-face :inherit 'font-lock-comment-face)))

Each face spec must be in the format of (FACE-NAME [:ATTRIBUTE VALUE]...).

Unlike `custom-set-faces', which destructively changes a face's spec, this one
adjusts pre-existing ones."
  `(add-hook
    'doom-load-theme-hook
    (let ((fn (make-symbol "doom|init-custom-faces")))
      (fset fn
            (lambda ()
              ,@(let (forms)
                  (dolist (spec-group spec-groups)
                    (if (keywordp (cadr spec-group))
                        (cl-destructuring-bind (face . attrs) spec-group
                          (push `(set-face-attribute ,(if (symbolp face) `(quote ,face) face)
                                                     nil ,@attrs)
                                forms))
                      (let ((package (car spec-group))
                            (specs (cdr spec-group)))
                        (push `(after! ,package
                                 ,@(cl-loop for (face . attrs) in specs
                                            collect `(set-face-attribute ,(if (symbolp face) `(quote ,face) face)
                                                                         nil ,@attrs)))
                              forms))))
                  (nreverse forms))))
      fn)
    'append))

(provide 'core-lib)
;;; core-lib.el ends here
