;;; core-lib.el -*- lexical-binding: t; -*-

;; Built in packages we use a lot of
(require 'subr-x)
(require 'cl-lib)

(eval-and-compile
  (unless EMACS26+
    (with-no-warnings
      ;; if-let and when-let are deprecated in Emacs 26+ in favor of their
      ;; if-let* variants, so we alias them for 25 users.
      (defalias 'if-let* #'if-let)
      (defalias 'when-let* #'when-let)

      ;; `alist-get' doesn't have its 5th argument before Emacs 26
      (defun doom*alist-get (key alist &optional default remove testfn)
        "Return the value associated with KEY in ALIST.
If KEY is not found in ALIST, return DEFAULT.
Use TESTFN to lookup in the alist if non-nil.  Otherwise, use `assq'.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT."
        (ignore remove) ;;Silence byte-compiler.
        (let ((x (if (not testfn)
                     (assq key alist)
                   (assoc key alist testfn))))
          (if x (cdr x) default)))
      (advice-add #'alist-get :override #'doom*alist-get))))


;;
;; Helpers
;;

(defun doom--resolve-path-forms (spec &optional directory)
  "Converts a simple nested series of or/and forms into a series of
`file-exists-p' checks.

For example

  (doom--resolve-path-forms
    '(or \"some-file\" (and path-var \"/an/absolute/path\"))
    \"~\")

Returns

  '(let ((_directory \"~\"))
     (or (file-exists-p (expand-file-name \"some-file\" _directory))
         (and (file-exists-p (expand-file-name path-var _directory))
              (file-exists-p \"/an/absolute/path\"))))

This is used by `associate!', `file-exists-p!' and `project-file-exists-p!'."
  (declare (pure t) (side-effect-free t))
  (cond ((stringp spec)
         `(file-exists-p
           ,(if (file-name-absolute-p spec)
                spec
              `(expand-file-name ,spec ,directory))))
        ((symbolp spec)
         `(file-exists-p ,(if (and directory
                                   (or (not (stringp directory))
                                       (file-name-absolute-p directory)))
                              `(expand-file-name ,spec ,directory)
                            spec)))
        ((and (listp spec)
              (memq (car spec) '(or and)))
         `(,(car spec)
           ,@(cl-loop for i in (cdr spec)
                      collect (doom--resolve-path-forms i directory))))
        ((listp spec)
         (doom--resolve-path-forms (eval spec t) directory))
        (t spec)))

(defun doom--resolve-hook-forms (hooks)
  (declare (pure t) (side-effect-free t))
  (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
           for hook in (doom-enlist (doom-unquote hooks))
           if (eq (car-safe hook) 'quote)
            collect (cadr hook)
           else if quoted-p
            collect hook
           else collect (intern (format "%s-hook" (symbol-name hook)))))

(defun doom--assert-stage-p (stage macro)
  (cl-assert (eq stage doom--stage)
             nil
             "Found %s call in non-%s.el file (%s)"
             macro (symbol-name stage)
             (let ((path (FILE!)))
               (if (file-in-directory-p path doom-emacs-dir)
                   (file-relative-name path doom-emacs-dir)
                 (abbreviate-file-name path)))))


;;
;; Functions
;;

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

(cl-defun doom-files-in
    (path-or-paths &rest rest
                   &key
                   filter
                   map
                   full
                   nosort
                   (follow-symlinks t)
                   (type 'files)
                   (relative-to (unless full default-directory))
                   (depth 99999)
                   (mindepth 0)
                   (match "^[^.]"))
  "Returns a list of files/directories in PATH-OR-PATHS (one string path or a
list of them).

FILTER is a function or symbol that takes one argument (the path). If it returns
non-nil, the entry will be excluded.

MAP is a function or symbol which will be used to transform each entry in the
results.

TYPE determines what kind of path will be included in the results. This can be t
(files and folders), 'files or 'dirs.

By default, this function returns paths relative to PATH-OR-PATHS if it is a
single path. If it a list of paths, this function returns absolute paths.
Otherwise, by setting RELATIVE-TO to a path, the results will be transformed to
be relative to it.

The search recurses up to DEPTH and no further. DEPTH is an integer.

MATCH is a string regexp. Only entries that match it will be included."
  (cond
   ((listp path-or-paths)
    (cl-loop for path in path-or-paths
             if (file-directory-p path)
             nconc (apply #'doom-files-in path (plist-put rest :relative-to relative-to))))
   ((let ((path path-or-paths)
          result)
      (when (file-directory-p path)
        (dolist (file (directory-files path nil "." nosort))
          (unless (member file '("." ".."))
            (let ((fullpath (expand-file-name file path)))
              (cond ((file-directory-p fullpath)
                     (when (and (memq type '(t dirs))
                                (string-match-p match fullpath)
                                (not (and filter (funcall filter fullpath)))
                                (not (and (file-symlink-p fullpath)
                                          (not follow-symlinks)))
                                (<= mindepth 0))
                       (setq result
                             (nconc result
                                    (list (cond (map (funcall map fullpath))
                                                (relative-to (file-relative-name fullpath relative-to))
                                                (fullpath))))))
                     (unless (< depth 1)
                       (setq result
                             (nconc result (apply #'doom-files-in fullpath
                                                  (append `(:mindepth ,(1- mindepth)
                                                            :depth ,(1- depth)
                                                            :relative-to ,relative-to)
                                                          rest))))))
                    ((and (memq type '(t files))
                          (string-match-p match fullpath)
                          (not (and filter (funcall filter fullpath)))
                          (<= mindepth 0))
                     (push (if relative-to
                               (file-relative-name fullpath relative-to)
                             fullpath)
                           result))))))
        result)))))

(defun doom*shut-up (orig-fn &rest args)
  "Generic advisor for silencing noisy functions."
  (quiet! (apply orig-fn args)))


;;
;; Macros
;;

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

(defmacro λ! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defalias 'lambda! 'λ!)

(defmacro defer-until! (condition &rest body)
  "Run BODY when CONDITION is true (checks on `after-load-functions'). Meant to
serve as a predicated alternative to `after!'."
  (declare (indent defun) (debug t))
  `(if ,condition
       (progn ,@body)
     ,(let ((fun (gensym "doom|delay-form-")))
        `(progn
           (fset ',fun (lambda (&rest args)
                         (when ,(or condition t)
                           (remove-hook 'after-load-functions #',fun)
                           (unintern ',fun nil)
                           (ignore args)
                           ,@body)))
           (put ',fun 'permanent-local-hook t)
           (add-hook 'after-load-functions #',fun)))))

(defmacro after! (targets &rest body)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation. This will no-op on features that have been disabled by the user."
  (declare (indent defun) (debug t))
  (unless (and (symbolp targets)
               (memq targets (bound-and-true-p doom-disabled-packages)))
    (list (if (or (not (bound-and-true-p byte-compile-current-file))
                  (dolist (next (doom-enlist targets))
                    (unless (keywordp next)
                      (if (symbolp next)
                          (require next nil :no-error)
                        (load next :no-message :no-error)))))
              #'progn
            #'with-no-warnings)
          (if (symbolp targets)
              `(with-eval-after-load ',targets ,@body)
            (pcase (car-safe targets)
              ((or :or :any)
               (macroexp-progn
                (cl-loop for next in (cdr targets)
                         collect `(after! ,next ,@body))))
              ((or :and :all)
               (dolist (next (cdr targets))
                 (setq body `((after! ,next ,@body))))
               (car body))
              (_ `(after! (:and ,@targets) ,@body)))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any output."
  `(if doom-debug-mode
       (progn ,@forms)
     (let ((old-fn (symbol-function 'write-region)))
       (cl-letf* ((standard-output (lambda (&rest _)))
                  ((symbol-function 'load-file) (lambda (file) (load file nil t)))
                  ((symbol-function 'message) (lambda (&rest _)))
                  ((symbol-function 'write-region)
                   (lambda (start end filename &optional append visit lockname mustbenew)
                     (unless visit (setq visit 'no-message))
                     (funcall old-fn start end filename append visit lockname mustbenew)))
                  (inhibit-message t)
                  (save-silently t))
         ,@forms))))

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        (fn (if (symbolp (car forms))
                (intern (format "doom|transient-hook-%s" (pop forms)))
              (gensym "doom|transient-hook-"))))
    `(progn
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (cond ((functionp ,hook-or-function) (advice-remove ,hook-or-function #',fn))
                     ((symbolp ,hook-or-function)   (remove-hook ,hook-or-function #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,hook-or-function)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp ,hook-or-function)
              (put ',fn 'permanent-local-hook t)
              (add-hook ,hook-or-function #',fn ,append))))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)   (same as `add-hook')
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
  "Convenience macro for `remove-hook'. Takes the same arguments as
`add-hook!'."
  (declare (indent defun) (debug t))
  `(add-hook! :remove ,@args))

(defmacro setq-hook! (hooks &rest rest)
  "Convenience macro for setting buffer-local variables in a hook.

  (setq-hook! 'markdown-mode-hook
    line-spacing 2
    fill-column 80)"
  (declare (indent 1))
  (unless (= 0 (% (length rest) 2))
    (signal 'wrong-number-of-arguments (length rest)))
  `(add-hook! ,hooks
     ,@(let (forms)
         (while rest
           (let ((var (pop rest))
                 (val (pop rest)))
             (push `(setq-local ,var ,val) forms)))
         (nreverse forms))))

(cl-defmacro associate! (mode &key modes match files when)
  "Enables a minor mode if certain conditions are met.

The available conditions are:

  :modes SYMBOL_LIST
    A list of major/minor modes in which this minor mode may apply.
  :match REGEXP
    A regexp to be tested against the current file path.
  :files SPEC
    Accepts what `project-file-exists-p!' accepts. Checks if certain files exist
    relative to the project root.
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
                             ,(if match `(if buffer-file-name (string-match-p ,match buffer-file-name)) t)
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
  "Returns t if the files in SPEC all exist.

SPEC can be a single file or a list of forms/files. It understands nested (and
...) and (or ...), as well.

DIRECTORY is where to look for the files in SPEC if they aren't absolute. This
doesn't apply to variables, however.

For example:

  (file-exists-p! (or doom-core-dir \"~/.config\" \"some-file\") \"~\")"
  (if directory
      `(let ((--directory-- ,directory))
         ,(doom--resolve-path-forms spec '--directory--))
    (doom--resolve-path-forms spec)))

(defmacro define-key! (keymaps key def &rest rest)
  "Like `define-key', but accepts a variable number of KEYMAPS and/or KEY+DEFs.

KEYMAPS can also be (or contain) 'global or 'local, to make this equivalent to
using `global-set-key' and `local-set-key'.

KEY is a key string or vector. It is *not* piped through `kbd'."
  (declare (indent defun))
  (or (cl-evenp (length rest))
      (signal 'wrong-number-of-arguments (list 'evenp (length rest))))
  (if (and (listp keymaps)
           (not (eq (car-safe keymaps) 'quote)))
      `(dolist (map (list ,@keymaps))
         ,(macroexpand `(define-key! map ,key ,def ,@rest)))
    (when (eq (car-safe keymaps) 'quote)
      (pcase (cadr keymaps)
        (`global (setq keymaps '(current-global-map)))
        (`local  (setq keymaps '(current-local-map)))
        (x (error "%s is not a valid keymap" x))))
    `(let ((map ,keymaps))
       (define-key map ,key ,def)
       ,@(let (forms)
           (while rest
             (let ((key (pop rest))
                   (def (pop rest)))
               (push `(define-key map ,key ,def) forms)))
           (nreverse forms)))))

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

(provide 'core-lib)
;;; core-lib.el ends here
