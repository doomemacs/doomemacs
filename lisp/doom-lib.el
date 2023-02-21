;;; doom-lib.el --- Doom's core standard library -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Custom error types
(define-error 'doom-error "An unexpected Doom error")
(define-error 'doom-nosync-error "Doom hasn't been initialized yet; did you remember to run 'doom sync' in the shell?" 'doom-error)
(define-error 'doom-core-error "Unexpected error in Doom's core" 'doom-error)
(define-error 'doom-hook-error "Error in a Doom startup hook" 'doom-error)
(define-error 'doom-autoload-error "Error in Doom's autoloads file" 'doom-error)
(define-error 'doom-user-error "Error caused by user's config or system" 'doom-error)
(define-error 'doom-module-error "Error in a Doom module" 'doom-error)
(define-error 'doom-package-error "Error with packages" 'doom-error)
(define-error 'doom-profile-error "Error while processing profiles" 'doom-error)


;;
;;; Logging

(defvar doom-inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `doom-log' output.")

(defun doom--log (text &rest args)
  (let ((inhibit-message (not init-file-debug))
        (absolute? (string-prefix-p ":" text)))
    (apply #'message
           (propertize (concat "* %.06f:%s" (if (not absolute?) ":") text)
                       'face 'font-lock-doc-face)
           (float-time (time-subtract (current-time) before-init-time))
           (mapconcat
            (lambda (x) (format "%s" x))
            (unless absolute?
              (append (cons '* (remq t (reverse doom-context)))
                      (if (bound-and-true-p doom-module-context)
                          (let ((key (doom-module-context-key)))
                            (delq nil (list (car key) (cdr key)))))))
            ":")
           args)))

(defmacro doom-log (message &rest args)
  "Log a message in *Messages*.

Does not emit the message in the echo area. This is a macro instead of a
function to prevent the potentially expensive evaluation of its arguments when
debug mode is off. Return non-nil."
  (declare (debug t))
  `(unless doom-inhibit-log (doom--log ,message ,@args)))


;;
;;; Helpers

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun doom--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (doom--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "doom--setq-%s-for-%s-h"
                                          var mode))))))


;;
;;; Public library

(define-obsolete-function-alias 'doom-enlist 'ensure-list "v3.0.0")

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom-keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun doom-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defalias 'doom-partial #'apply-partially)

(defun doom-rpartial (fn &rest args)
  "Return a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun doom-lookup-key (keys &rest keymaps)
  "Like `lookup-key', but search active keymaps if KEYMAP is omitted."
  (if keymaps
      (cl-some (doom-rpartial #'lookup-key keys) keymaps)
    (cl-loop for keymap
             in (append (cl-loop for alist in emulation-mode-map-alists
                                 append (mapcar #'cdr
                                                (if (symbolp alist)
                                                    (if (boundp alist) (symbol-value alist))
                                                  alist)))
                        (list (current-local-map))
                        (mapcar #'cdr minor-mode-overriding-map-alist)
                        (mapcar #'cdr minor-mode-map-alist)
                        (list (current-global-map)))
             if (keymapp keymap)
             if (lookup-key keymap keys)
             return it)))

(defun doom-load (path &optional noerror)
  "Load PATH and handle any Doom errors that arise from it.

If NOERROR, don't throw an error if PATH doesn't exist."
  (doom-log "load: %s %s" (abbreviate-file-name path) noerror)
  (condition-case-unless-debug e
      (load path noerror 'nomessage)
    (doom-error
     (signal (car e) (cdr e)))
    (error
     (setq path (locate-file path load-path (get-load-suffixes)))
     (signal (cond ((not (and path (featurep 'doom)))
                    'error)
                   ((file-in-directory-p path (expand-file-name "cli" doom-core-dir))
                    'doom-cli-error)
                   ((file-in-directory-p path doom-core-dir)
                    'doom-core-error)
                   ((file-in-directory-p path doom-user-dir)
                    'doom-user-error)
                   ((file-in-directory-p path doom-profile-dir)
                    'doom-profile-error)
                   ((file-in-directory-p path doom-modules-dir)
                    'doom-module-error)
                   ('doom-error))
             (list path e)))))

(defun doom-require (feature &optional filename noerror)
  "Like `require', but handles and enhances Doom errors.

Can also load Doom's subfeatures, e.g. (doom-require 'doom-lib 'files)"
  (let ((subfeature (if (symbolp filename) filename)))
    (or (featurep feature subfeature)
        (doom-load
         (if subfeature
             (file-name-concat doom-core-dir
                               (string-remove-prefix "doom-" (symbol-name feature))
                               (symbol-name filename))
           (symbol-name feature))
         noerror))))

(defun doom-load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (null (file-exists-p file))
      (unless noerror
        (signal 'file-error (list "No envvar file exists" file)))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let (env (read (current-buffer)))
        (let ((tz (getenv-internal "TZ")))
          (setq-default
           process-environment
           (append env (default-value 'process-environment))
           exec-path
           (append (split-string (getenv "PATH") path-separator t)
                   (list exec-directory))
           shell-file-name
           (or (getenv "SHELL")
               (default-value 'shell-file-name)))
          (when-let (newtz (getenv-internal "TZ"))
            (unless (equal tz newtz)
              (set-time-zone-rule newtz))))
        env))))

(defvar doom--hook nil)
(defun doom-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (doom-log "hook:%s: run %s" (or doom--hook '*) hook)
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'doom-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun doom-run-hooks (&rest hooks)
  "Run HOOKS (a list of hook variable symbols) with better error handling.
Is used as advice to replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (let ((doom--hook hook))
          (run-hook-wrapped hook #'doom-run-hook))
      (doom-hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e))
                    (symbol-name (cadr e))
                  (cadr e))
                (caddr e)))
       (signal 'doom-hook-error (cons hook (cdr e)))))))

(defun doom-run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked *after* Emacs has initialized (to reduce false positives). Once
HOOK-VAR is triggered, it is reset to nil.

HOOK-VAR is a quoted hook.
TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook)))
          running?)
      (fset
       fn (lambda (&rest _)
            ;; Only trigger this after Emacs has initialized.
            (when (and after-init-time
                       (not running?)
                       (or (daemonp)
                           ;; In some cases, hooks may be lexically unset to
                           ;; inhibit them during expensive batch operations on
                           ;; buffers (such as when processing buffers
                           ;; internally). In these cases we should assume this
                           ;; hook wasn't invoked interactively.
                           (and (boundp hook)
                                (symbol-value hook))))
              (setq running? t)  ; prevent infinite recursion
              (doom-run-hooks hook-var)
              (set hook-var nil))))
      (cond ((daemonp)
             ;; In a daemon session we don't need all these lazy loading
             ;; shenanigans. Just load everything immediately.
             (add-hook 'after-init-hook fn 'append))
            ((eq hook 'find-file-hook)
             ;; Advise `after-find-file' instead of using `find-file-hook'
             ;; because the latter is triggered too late (after the file has
             ;; opened and modes are all set up).
             (advice-add 'after-find-file :before fn '((depth . -101))))
            ((add-hook hook fn -101)))
      fn)))

(defun doom-compile-functions (&rest fns)
  "Queue FNS to be byte/natively-compiled after a brief delay."
  (with-memoization (get 'doom-compile-function 'timer)
    (run-with-idle-timer
     1.5 t (fn! (when-let (fn (pop fns))
                  (doom-log "compile-functions: %s" fn)
                  (or (if (featurep 'native-compile)
                          (or (subr-native-elisp-p (indirect-function fn))
                              (ignore-errors (native-compile fn))))
                      (byte-code-function-p fn)
                      (let (byte-compile-warnings)
                        (byte-compile fn))))
                (unless fns
                  (cancel-timer (get 'doom-compile-function 'timer))
                  (put 'doom-compile-function 'timer nil))))))


;;
;;; Sugars

(defmacro file! ()
  "Return the file of the file this macro was called."
  (or
   ;; REVIEW: Use `macroexp-file-name' once 27 support is dropped.
   (let ((file (car (last current-load-list))))
     (if (stringp file) file))
   (bound-and-true-p byte-compile-current-file)
   load-file-name
   buffer-file-name   ; for `eval'
   (error "file!: cannot deduce the current file path")))

(defmacro dir! ()
  "Return the directory of the file this macro was called."
   (let (file-name-handler-alist)
     (file-name-directory (macroexpand '(file!)))))

;; REVIEW Should I deprecate this? The macro's name is so long...
(defalias 'letenv! 'with-environment-variables)

(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.

Intended as syntax sugar for `cl-letf', `cl-labels', `cl-macrolet', and
temporary advice.

BINDINGS is either:

  A list of, or a single, `defun', `defun*', `defmacro', or `defadvice' forms.
  A list of (PLACE VALUE) bindings as `cl-letf*' would accept.

TYPE is one of:

  `defun' (uses `cl-letf')
  `defun*' (uses `cl-labels'; allows recursive references),
  `defmacro' (uses `cl-macrolet')
  `defadvice' (uses `defadvice!' before BODY, then `undefadvice!' after)

NAME, ARGLIST, and BODY are the same as `defun', `defun*', `defmacro', and
`defadvice!', respectively.

\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defadvice `(progn (defadvice! ,@rest)
                                  (unwind-protect ,body (undefadvice! ,@rest))))
              ((or `defun `defun*)
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  ,(if (eq type 'defun*)
                       `(cl-labels ((,@rest)) ,body)
                     `(cl-letf (((symbol-function #',(car rest))
                                 (lambda! ,(cadr rest) ,@(cddr rest))))
                        ,body))))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and anything that
writes to `standard-output'. In interactive sessions this inhibits output to the
echo-area, but not to *Messages*."
  `(if init-file-debug
       (progn ,@forms)
     ,(if noninteractive
          `(letf! ((standard-output (lambda (&rest _)))
                   (defun message (&rest _))
                   (defun load (file &optional noerror nomessage nosuffix must-suffix)
                     (funcall load file noerror t nosuffix must-suffix))
                   (defun write-region (start end filename &optional append visit lockname mustbenew)
                     (unless visit (setq visit 'no-message))
                     (funcall write-region start end filename append visit lockname mustbenew)))
             ,@forms)
        `(let ((inhibit-message t)
               (save-silently t))
           (prog1 ,@forms (message ""))))))

(defmacro eval-if! (cond then &rest body)
  "Expands to THEN if COND is non-nil, to BODY otherwise.
COND is checked at compile/expansion time, allowing BODY to be omitted entirely
when the elisp is byte-compiled. Use this for forms that contain expensive
macros that could safely be removed at compile time."
  (declare (indent 2))
  (if (eval cond)
      then
    (macroexp-progn body)))

(defmacro eval-when! (cond &rest body)
  "Expands to BODY if CONDITION is non-nil at compile/expansion time.
See `eval-if!' for details on this macro's purpose."
  (declare (indent 1))
  (when (eval cond)
    (macroexp-progn body)))

(defmacro versionp! (v1 comp v2 &rest comps)
  "Perform compound version checks.

Compares V1 and V2 with COMP (a math comparison operator: <, <=, =, /=, >=, >).
Can chain these comparisons by adding more (COMPn Vn) pairs afterwards.

\(fn V1 COMP V2 [COMPn Vn]...)"
  (let ((forms t))
    (push v2 comps)
    (push comp comps)
    `(let ((v2 (version-to-list ,v1)))
       ,(progn
          (cl-loop for (v op) on (nreverse comps) by #'cddr
                   for not? = (not (memq op '(> >= /=)))
                   for fn = (or (get 'versionp! op)
                                (error "Invalid comparator %s" op))
                   for form = `(,fn v1 v2)
                   do (if not? (setq form `(not ,form)))
                   do (setq v1 'v2
                            v2 `(version-to-list ,v)
                            forms `(let ((v1 ,v1)
                                         (v2 ,v2))
                                     (and (not ,form) ,forms))))
          forms))))
;; PERF: Store in symbol plist for ultra-fast lookups at this scale.
(setplist 'versionp! '(>  version-list-<
                       >= version-list-<=
                       <  version-list-<
                       <= version-list-<=
                       =  version-list-=
                       /= version-list-=))

;;; Closure factories
(defmacro lambda! (arglist &rest body)
  "Returns (cl-function (lambda ARGLIST BODY...))
The closure is wrapped in `cl-function', meaning ARGLIST will accept anything
`cl-defun' will. Implicitly adds `&allow-other-keys' if `&key' is present in
ARGLIST."
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t))
  `(cl-function
    (lambda
      ,(letf! (defun* allow-other-keys (args)
                (mapcar
                 (lambda (arg)
                   (cond ((nlistp (cdr-safe arg)) arg)
                         ((listp arg) (allow-other-keys arg))
                         (arg)))
                 (if (and (memq '&key args)
                          (not (memq '&allow-other-keys args)))
                     (if (memq '&aux args)
                         (let (newargs arg)
                           (while args
                             (setq arg (pop args))
                             (when (eq arg '&aux)
                               (push '&allow-other-keys newargs))
                             (push arg newargs))
                           (nreverse newargs))
                       (append args (list '&allow-other-keys)))
                   args)))
         (allow-other-keys arglist))
      ,@body)))

(setplist 'doom--fn-crawl '(%2 2 %3 3 %4 4 %5 5 %6 6 %7 7 %8 8 %9 9))
(defun doom--fn-crawl (data args)
  (cond ((symbolp data)
         (when-let
             (pos (cond ((eq data '%*) 0)
                        ((memq data '(% %1)) 1)
                        ((get 'doom--fn-crawl data))))
           (when (and (= pos 1)
                      (aref args 1)
                      (not (eq data (aref args 1))))
             (error "%% and %%1 are mutually exclusive"))
           (aset args pos data)))
        ((and (not (eq (car-safe data) 'fn!))
              (or (listp data)
                  (vectorp data)))
         (let ((len (length data))
               (i 0))
           (while (< i len)
             (doom--fn-crawl (elt data i) args)
             (cl-incf i))))))

(defmacro fn! (&rest args)
  "Return an lambda with implicit, positional arguments.

The function's arguments are determined recursively from ARGS.  Each symbol from
`%1' through `%9' that appears in ARGS is treated as a positional argument.
Missing arguments are named `_%N', which keeps the byte-compiler quiet.  `%' is
a shorthand for `%1'; only one of these can appear in ARGS.  `%*' represents
extra `&rest' arguments.

Instead of:

  (lambda (a _ c &rest d)
    (if a c (cadr d)))

you can use this macro and write:

  (fn! (if %1 %3 (cadr %*)))

which expands to:

  (lambda (%1 _%2 %3 &rest %*)
    (if %1 %3 (cadr %*)))

This macro was adapted from llama.el (see https://git.sr.ht/~tarsius/llama),
minus font-locking and the outer function call, plus some minor optimizations."
  `(lambda ,(let ((argv (make-vector 10 nil)))
              (doom--fn-crawl args argv)
              `(,@(let ((i (1- (length argv)))
                        (n -1)
                        sym arglist)
                    (while (> i 0)
                      (setq sym (aref argv i))
                      (unless (and (= n -1) (null sym))
                        (cl-incf n)
                        (push (or sym (intern (format "_%%%d" i)))
                              arglist))
                      (cl-decf i))
                    arglist)
                ,@(and (aref argv 0) '(&rest %*))))
     ,@args))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional prefix-arg &rest args)
  "Returns a closure that interactively calls COMMAND with ARGS and PREFIX-ARG.
Like `cmd!', but allows you to change `current-prefix-arg' or pass arguments to
COMMAND. This macro is meant to be used as a target for keybinds (e.g. with
`define-key' or `map!')."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,prefix-arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        ,command ,@args))))

(defmacro cmds! (&rest branches)
  "Returns a dispatcher that runs the a command in BRANCHES.
Meant to be used as a target for keybinds (e.g. with `define-key' or `map!').

BRANCHES is a flat list of CONDITION COMMAND pairs. CONDITION is a lisp form
that is evaluated when (and each time) the dispatcher is invoked. If it returns
non-nil, COMMAND is invoked, otherwise it falls through to the next pair.

The last element of BRANCHES can be a COMMANd with no CONDITION. This acts as
the fallback if all other conditions fail.

Otherwise, Emacs will fall through the keybind and search the next keymap for a
keybind (as if this keybind never existed).

See `general-key-dispatch' for what other arguments it accepts in BRANCHES."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    (let ((defs (cl-loop for (key value) on branches by 'cddr
                         unless (keywordp key)
                         collect (list key value))))
      `'(menu-item
         ,(or docstring "") nil
         :filter (lambda (&optional _)
                   (let (it)
                     (cond ,@(mapcar (lambda (pred-def)
                                       `((setq it ,(car pred-def))
                                         ,(cadr pred-def)))
                                     defs)
                           (t ,fallback))))))))

(defalias 'kbd! #'general-simulate-key)

;; For backwards compatibility
(defalias 'λ!  #'cmd!)
(defalias 'λ!! #'cmd!!)


;;; Mutation
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro setq! (&rest settings)
  "A more sensible `setopt' for setting customizable variables.

This can be used as a drop-in replacement for `setq' and *should* be used
instead of `setopt'. Unlike `setq', this triggers custom setters on variables.
Unlike `setopt', this won't needlessly pull in dependencies."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                              ',var ,val))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
                          `(funcall ,fetcher ,elt ,list)
                        elt)
                     ,list)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))


;;; Loading
(defmacro add-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file.
The current file is the file from which `add-to-load-path!' is used."
  `(let ((default-directory (dir!))
         file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path :test #'string=))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features (aka
packages). PACKAGE may use :or/:any and :and/:all operators. The precise format
is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. No-ops for package that are disabled by the user (via `package!') or not
   installed yet.
2. Supports compound package statements (see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p doom-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  `(doom-load
    (file-name-concat ,(or path `(dir!)) ,filename)
    ,noerror))

(defmacro defer-until! (condition &rest body)
  "Run BODY when CONDITION is true (checks on `after-load-functions'). Meant to
serve as a predicated alternative to `after!'."
  (declare (indent defun) (debug t))
  `(if ,condition
       (progn ,@body)
     ,(let ((fn (intern (format "doom--delay-form-%s-h" (sxhash (cons condition body))))))
        `(progn
           (fset ',fn (lambda (&rest args)
                        (when ,(or condition t)
                          (remove-hook 'after-load-functions #',fn)
                          (unintern ',fn nil)
                          (ignore args)
                          ,@body)))
           (put ',fn 'permanent-local-hook t)
           (add-hook 'after-load-functions #',fn)))))

(defmacro defer-feature! (feature &rest fns)
  "Pretend FEATURE hasn't been loaded yet, until FEATURE-hook or FNS run.

Some packages (like `elisp-mode' and `lisp-mode') are loaded immediately at
startup, which will prematurely trigger `after!' (and `with-eval-after-load')
blocks. To get around this we make Emacs believe FEATURE hasn't been loaded yet,
then wait until FEATURE-hook (or any of FNS, if FNS are provided) is triggered
to reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "doom--defer-feature-%s-a" feature)))
        (fns (or fns (list feature))))
    `(progn
       (delq! ',feature features)
       (defadvice! ,advice-fn (&rest _)
         :before ',fns
         ;; Some plugins (like yasnippet) will invoke a fn early to parse
         ;; code, which would prematurely trigger this. In those cases, well
         ;; behaved plugins will use `delay-mode-hooks', which we can check for:
         (unless delay-mode-hooks
           ;; ...Otherwise, announce to the world this package has been loaded,
           ;; so `after!' handlers can react.
           (provide ',feature)
           (dolist (fn ',fns)
             (advice-remove fn #',advice-fn)))))))


;;; Hooks
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append? (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "doom-transient-hook")))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (doom-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append? :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append?))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook (nreverse ',hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (doom--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (doom--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))


;;; Definers
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.

This has the same signature as `defadvice!' an exists as an easy undefiner when
testing advice (when combined with `rotate-text').

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

(defmacro defbackport! (type symbol &rest body)
  "Backport a function/macro/alias from later versions of Emacs."
  (declare (indent defun) (doc-string 4))
  (unless (fboundp (doom-unquote symbol))
    `(,type ,symbol ,@body)))


;;
;;; Backports

;; `format-spec' wasn't autoloaded until 28.1
(defbackport! autoload 'format-spec "format-spec")

;; Introduced in Emacs 28.1
(defbackport! defun ensure-list (object)
  "Return OBJECT as a list.
If OBJECT is already a list, return OBJECT itself.  If it's
not a list, return a one-element list containing OBJECT."
  (declare (pure t) (side-effect-free t))
  (if (listp object) object (list object)))

;; Introduced in Emacs 28.1
(defbackport! defun always (&rest _args)
  "Do nothing and return t.
This function accepts any number of ARGUMENTS, but ignores them.
Also see `ignore'."
  t)

;; Introduced in Emacs 28.1
(defbackport! defun file-name-concat (directory &rest components)
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
   "/"))

;; Introduced in Emacs 28.1
(defbackport! defmacro with-environment-variables (variables &rest body)
  "Set VARIABLES in the environment and execute BODY.
VARIABLES is a list of variable settings of the form (VAR VALUE),
where VAR is the name of the variable (a string) and VALUE
is its value (also a string).

The previous values will be be restored upon exit."
  (declare (indent 1) (debug (sexp body)))
  (unless (consp variables)
    (error "Invalid VARIABLES: %s" variables))
  `(let ((process-environment (copy-sequence process-environment)))
     ,@(cl-loop for var in variables
                collect `(setenv ,(car var) ,(cadr var)))
     ,@body))

;; Introduced in Emacs 28.1
(defbackport! defun file-name-with-extension (filename extension)
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
          ((concat (file-name-sans-extension filename) "." extn)))))

;; Introduced in Emacs 29+
(defbackport! defmacro with-memoization (place &rest code)
  "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
  (declare (indent 1) (debug (gv-place body)))
  (gv-letplace (getter setter) place
    `(or ,getter
         ,(macroexp-let2 nil val (macroexp-progn code)
            `(progn
               ,(funcall setter val)
               ,val)))))

;; Introduced in Emacs 29+ (emacs-mirror/emacs@f117b5df4dc6)
(defbackport! defalias 'bol #'line-beginning-position)
(defbackport! defalias 'eol #'line-end-position)

(provide 'doom-lib)
;;; doom-lib.el ends here
