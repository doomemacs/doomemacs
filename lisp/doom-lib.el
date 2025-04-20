;;; doom-lib.el --- Doom's core standard library -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Custom error types
(define-error 'doom-error "An unexpected Doom error")
(dolist (type '((doom-font-error "Could not find a font on your system" doom-error)
                (doom-nosync-error "Doom hasn't been initialized yet; did you remember to run 'doom sync' in the shell?" doom-error)
                (doom-core-error "Unexpected error in Doom's core" doom-error)
                (doom-cli-error "Unexpected error in Doom's CLI" doom-error)
                (doom-context-error "Incorrect context error" doom-error)
                (doom-hook-error "Error in a Doom startup hook" doom-error)
                (doom-autoload-error "Error in Doom's autoloads file" doom-error)
                (doom-user-error "Error caused by user's config or system" doom-error)
                (doom-profile-error "Error while processing profiles" doom-error)
                (doom-module-error "Error in a Doom module" doom-profile-error)
                (doom-source-error "Error in a Doom source" doom-profile-error)
                (doom-package-error "Error with packages" doom-profile-error)))
  (apply #'define-error type)
  (fset (car type) (lambda (&rest data) (signal (car type) data))))

(defmacro doom-error (type &rest data)
  "Signal a Doom error of TYPE with DATA.

TYPE should be a keyword of any of the known doom-*-error errors (e.g. :font,
:module, etc), or the name of any error."
  `(signal ,(if (keywordp type)
                `(quote
                  ,(or (intern-soft (format "doom-%s-error" (doom-keyword-name type)))
                       (doom-core-error "Invalid error type" type)))
              type)
           (list ,@data)))


;;
;;; Logging

(defvar doom-inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `doom-log' output completely.")

(defvar doom-log-level
  (if init-file-debug
      (if-let* ((level (getenv-internal "DEBUG"))
                (level (if (string-empty-p level) 1 (string-to-number level)))
                ((not (zerop level))))
          level
        2)
    0)
  "How verbosely to log from `doom-log' calls.

0 -- No logging at all.
1 -- Only warnings.
2 -- Warnings and notices.
3 -- Debug info, warnings, and notices.")

(defun doom--log (level text &rest args)
  (let ((inhibit-message (if noninteractive
                             (not init-file-debug)
                           (> level doom-log-level)))
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
                          (let ((key (doom-module-context-key doom-module-context)))
                            (delq nil (list (car key) (cdr key)))))))
            ":")
           args)))

;; This is a macro instead of a function to prevent the potentially expensive
;; evaluation of its arguments when debug mode is off. Return non-nil.
(defmacro doom-log (message &rest args)
  "Log a message to stderr or *Messages* (without displaying in the echo area)."
  (declare (debug t))
  (let ((level (if (integerp message)
                   (prog1 message
                     (setq message (pop args)))
                 2)))
    `(when (and (not doom-inhibit-log)
                (or (not noninteractive)
                    (<= ,level doom-log-level)))
       (doom--log ,level ,message ,@args))))


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

(define-obsolete-function-alias 'doom-enlist 'ensure-list "3.0.0")

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

If NOERROR, don't throw an error if PATH doesn't exist.
Return non-nil if loading the file succeeds."
  (doom-log "load: %s %s" (abbreviate-file-name path) noerror)
  (condition-case-unless-debug e
      (load path noerror 'nomessage)
    (doom-error
     (signal (car e) (cdr e)))
    (error
     (setq path (locate-file path load-path (get-load-suffixes)))
     (if (not (and path (featurep 'doom)))
         (signal (car e) (cdr e))
       (cl-loop for (err . dir)
                in `((doom-cli-error     . ,(expand-file-name "cli" doom-core-dir))
                     (doom-core-error    . ,doom-core-dir)
                     (doom-user-error    . ,doom-user-dir)
                     (doom-profile-error . ,doom-profile-dir)
                     (doom-module-error  . ,doom-modules-dir))
                if (file-in-directory-p path dir)
                do (signal err (list (file-relative-name path (expand-file-name "../" dir))
                                     e)))))))

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
  (doom-log 2 "hook:%s: run %s" (or doom--hook '*) hook)
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
            (when (and (not running?)
                       (not (doom-context-p 'startup))
                       (or (daemonp)
                           ;; In some cases, hooks may be lexically unset to
                           ;; inhibit them during expensive batch operations on
                           ;; buffers (such as when processing buffers
                           ;; internally). In that case assume this hook was
                           ;; invoked non-interactively.
                           (and (boundp hook)
                                (symbol-value hook))))
              (setq running? t)  ; prevent infinite recursion
              (doom-run-hooks hook-var)
              (set hook-var nil))))
      (when (daemonp)
        ;; In a daemon session we don't need all these lazy loading shenanigans.
        ;; Just load everything immediately.
        (add-hook 'server-after-make-frame-hook fn 'append))
      (if (eq hook 'find-file-hook)
          ;; Advise `after-find-file' instead of using `find-file-hook' because
          ;; the latter is triggered too late (after the file has opened and
          ;; modes are all set up).
          (advice-add 'after-find-file :before fn '((depth . -101)))
        (add-hook hook fn -101))
      fn)))

(defun doom-compile-functions (&rest fns)
  "Queue FNS to be byte/natively-compiled after a brief delay."
  (with-memoization (get 'doom-compile-function 'timer)
    (run-with-idle-timer
     1.5 t (fn! (when-let (fn (pop fns))
                  (doom-log 3 "compile-functions: %s" fn)
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
;;; Deep copying

(cl-defgeneric doom-copy (val &optional deep?)
  "Return a (optionally deep) copy of VAL."
  (if (recordp val)  ; `record' specializer not supported until Emacs 30
      (if deep?
          (cl-loop with newval = (copy-sequence val)
                   for idx from 1 to (length (cdr (cl-struct-slot-info (type-of val))))
                   do (aset newval idx (doom-copy (aref newval idx) t))
                   finally return newval)
        (copy-sequence val))
    (cl-check-type val (or integer float boolean symbol null))
    val))

(cl-defmethod doom-copy ((val sequence) &optional deep?)
  "Return a (optionally deep) copy of sequence VAL."
  (if (stringp val)
      (if deep? val (purecopy val))
    (if deep?
        (when-let ((newval (mapcar (doom-rpartial #'doom-copy t) val)))
          (if (vectorp val)
              (apply #'vector newval)
            newval))
      (copy-sequence val))))

(cl-defmethod doom-copy ((val cons) &optional deep?)
  "Return a (optionally deep) copy of cons cell/list VAL."
  (cons (doom-copy (car val) deep?)
        (doom-copy (cdr val) deep?)))

(cl-defmethod doom-copy ((val hash-table) &optional deep?)
  "Return a (optionally deep) copy of hash table VAL."
  (let ((table (copy-hash-table val)))
    (when deep?
      (maphash (lambda (key val)
                 (puthash key (doom-copy val t) table))
               table))
    table))


;;
;;; Sugars

(defmacro file! ()
  "Return the file of the file this macro was called."
  (or (bound-and-true-p byte-compile-current-file)
      load-file-name
      (buffer-file-name (buffer-base-buffer))  ; for `eval'
      ;; REVIEW: Use `macroexp-file-name' once 27 support is dropped.
      (let ((file (car (last current-load-list))))
        (if (stringp file) file))
      (error "file!: cannot deduce the current file path")))

(defmacro dir! ()
  "Return the directory of the file in which this macro was called."
  (let (file-name-handler-alist)
    (file-name-directory (macroexpand '(file!)))))

(define-obsolete-function-alias 'letenv! 'with-environment-variables "3.0.0")

(put 'defun* 'lisp-indent-function 'defun)
(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.

Intended as syntax sugar for `cl-letf', `cl-labels', `cl-macrolet', and
temporary advice (`define-advice').

BINDINGS is either:

  A list of (PLACE VALUE) bindings as `cl-letf*' would accept.
  A list of, or a single, `defun', `defun*', `defmacro', or `defadvice' forms.

The def* forms accepted are:

  (defun NAME (ARGS...) &rest BODY)
    Defines a temporary function with `cl-letf'
  (defun* NAME (ARGS...) &rest BODY)
    Defines a temporary function with `cl-labels' (allows recursive
    definitions).
  (defmacro NAME (ARGS...) &rest BODY)
    Uses `cl-macrolet'.
  (defadvice FUNCTION WHERE ADVICE)
    Uses `advice-add' (then `advice-remove' afterwards).
  (defadvice FUNCTION (HOW LAMBDA-LIST &optional NAME DEPTH) &rest BODY)
    Defines temporary advice with `define-advice'."
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
              (`defadvice
               (if (keywordp (cadr rest))
                   (cl-destructuring-bind (target where fn) rest
                     `(when-let (fn ,fn)
                        (advice-add ,target ,where fn)
                        (unwind-protect ,body (advice-remove ,target fn))))
                 (let* ((fn (pop rest))
                        (argspec (pop rest)))
                   (when (< (length argspec) 3)
                     (setq argspec
                           (list (nth 0 argspec)
                                 (nth 1 argspec)
                                 (or (nth 2 argspec) (gensym (format "%s-a" (symbol-name fn)))))))
                   (let ((name (nth 2 argspec)))
                     `(progn
                        (define-advice ,fn ,argspec ,@rest)
                        (unwind-protect ,body
                          (advice-remove #',fn #',name)
                          ,(if name `(fmakunbound ',name))))))))
              (`defun
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  (cl-letf (((symbol-function #',(car rest))
                             (lambda! ,(cadr rest) ,@(cddr rest))))
                    ,body)))
              (`defun*
               `(cl-labels ((,@rest)) ,body))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))


(defmacro quiet!! (&rest forms)
  "Run FORMS without generating any output (for real).

Unlike `quiet!', which will only suppress output in the echo area in interactive
sessions, this truly suppress all output from FORMS."
  (declare (indent 0))
  `(if init-file-debug
       (progn ,@forms)
     (letf! ((standard-output (lambda (&rest _)))
             (defun message (&rest _))
             (defun load (file &optional noerror nomessage nosuffix must-suffix)
               (funcall load file noerror t nosuffix must-suffix))
             (defun write-region (start end filename &optional append visit lockname mustbenew)
               (unless visit (setq visit 'no-message))
               (funcall write-region start end filename append visit lockname mustbenew)))
       ,@forms)))

(defmacro quiet! (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and anything that
writes to `standard-output'. In interactive sessions this inhibits output to the
echo-area, but not to *Messages*."
  (declare (indent 0))
  `(if init-file-debug
       (progn ,@forms)
     ,(if noninteractive
          `(quiet!! ,@forms)
        `(let ((inhibit-message t)
               (save-silently t))
           (prog1 ,@forms (message ""))))))

(define-obsolete-function-alias 'eval-if! 'static-if "3.0.0")
(define-obsolete-function-alias 'eval-when! 'static-when "3.0.0")

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
  (declare (doc-string 1))
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

(pcase-defmacro doom-struct (type &rest fields)
  `(and (pred (cl-struct-p))
        ;; TODO: Support `&rest', `&key', and `&optional' in FIELDS
        ,@(mapcar
           (lambda (field)
             (let ((offset (cl-struct-slot-offset type field)))
               `(app (lambda (it)
                       ,(if offset
                            `(aref it ,offset)
                          `(,(intern (format "%s-%s" ',type ',field)) it)))
                     ,field)))
           fields)))

(pcase-defmacro doom-module-context (&rest fields)
  `(doom-struct doom-module-context ,@fields))

(pcase-defmacro doom-module (&rest fields)
  `(doom-struct doom-module ,@fields))


;;; Mutation
;; DEPRECATED: Remove in v3.0
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  (declare (obsolete "Use `cl-callf2' instead" "3.0.0"))
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

;; DEPRECATED: Remove in v3.0
(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  (declare (obsolete "Use `cl-callf2' or `alist-get' instead" "3.0.0"))
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

;; DEPRECATED: Remove in v3.0
(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  (declare (obsolete "Use `cl-callf2' instead" "3.0.0"))
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
  and diff-hl have loaded)
    (after! (magit diff-hl) BODY...)
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
       (cl-callf2 delq ',feature features)
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
       (dolist (hook ',(nreverse hook-forms))
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
                       ,(format "%s = %s" var
                                (let ((print-level nil)
                                      (print-length nil))
                                  (prin1-to-string val)))
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

This has the same signature as `defadvice!' and exists as an easy undefiner when
interactively testing (and toggling) advice.

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


;;; Types

(cl-defstruct doom-module
  "TODO"
  (index 0 :read-only t)
  ;; source
  group
  name
  depth
  flags
  features
  ;; sources
  path
  ;; disabled-p
  ;; frozen-p
  ;; layer-p
  ;; recipe
  ;; alist
  ;; package
  ;; if
  )

(cl-defstruct doom-module-context
  "Hot cache object for the containing Doom module."
  index key path flags features)


;;; `doom-context'

(defvar doom-context '(t)
  "A list of symbols identifying all active Doom execution contexts.

This should never be directly changed, only let-bound, and should never be
empty. Each context describes what phase Doom is in, and may respond to.

Use `with-doom-context' instead of let-binding or setting this variable
directly.

All valid contexts:
  cli        -- executing a Doom CLI or doomscript
  emacs      -- in an interactive doom session
  module     -- loading any modules' elisp files

  Universal sub-contexts:
    compile    -- byte-compiling elisp
    startup    -- while doom is starting up, before any user config
    error      -- while Doom is in an error state

  `emacs' sub-contexts:
    docs       -- while rendering docs in `doom-docs-mode'
    reload     -- while reloading doom with `doom/reload'
    sandbox    -- this session was launched from Doom's sandbox
    eval       -- while interactively evaluating elisp

  `module' sub-contexts:
    external   -- loading packages or modules outside of $EMACSDIR or $DOOMDIR
    config     -- loading a module's config.el or cli.el
    doctor     -- loading a module's doctor.el
    init       -- loading a module's init.el
    package    -- loading a module's packages.el or managing packages
    source     -- while initializing a module source
    test       -- preparing for or running Doom's unit tests

  `cli' sub-contexts:
    run        -- running a CLI command")
(put 'doom-context 'valid
     '(compile error startup emacs docs reload sandbox eval module external
       config doctor init package test cli run))
(put 'doom-context 'risky-local-variable t)

(defun doom-context-p (contexts)
  "Return t if all CONTEXTS are active, nil otherwise.

See `doom-context' for possible values for CONTEXT."
  (declare (side-effect-free t))
  (catch 'result
    (let (result)
      (dolist (context (ensure-list contexts) result)
        (if (memq context doom-context)
            (push context result)
          (throw 'result nil))))))

(defun doom-context-valid-p (context)
  "Return non-nil if CONTEXT is a valid `doom-context'."
  (declare (pure t) (side-effect-free error-free))
  (memq context (get 'doom-context 'valid)))

(defun doom-context-push (contexts)
  "Add CONTEXTS to `doom-context', if not present.

Return list of successfully added contexts. Throws a `doom-context-error' if
CONTEXTS contains invalid contexts."
  (let ((contexts (ensure-list contexts)))
    (if (cl-loop for context in contexts
                 unless (doom-context-valid-p context)
                 return t)
        (doom-context-error
         (cl-remove-if #'doom-context-valid-p contexts)
         "Unrecognized context")
      (let (added)
        (dolist (context contexts)
          (unless (memq context doom-context)
            (push context added)))
        (when added
          (setq doom-context (nconc added doom-context))
          (doom-log 3 ":context: +%s %s" added doom-context)
          added)))))

(defun doom-context-pop (contexts)
  "Remove CONTEXTS from `doom-context'.

Return list of removed contexts if successful. Throws `doom-context-error' if
one of CONTEXTS isn't active."
  (if (not (doom-context-p contexts))
      (doom-context-error
       doom-context "Attempt to pop missing context"
       contexts)
    (let ((current-context (copy-sequence doom-context))
          removed)
      (dolist (context (ensure-list contexts))
        (setq current-context (delq context current-context))
        (push context removed))
      (when removed
        (setq doom-context current-context)
        (doom-log 3 ":context: +%s %s" removed doom-context)
        removed))))

(defmacro with-doom-context (contexts &rest body)
  "Evaluate BODY with CONTEXTS added to `doom-context'."
  (declare (indent 1))
  `(let ((doom-context doom-context))
     (doom-context-push ,contexts)
     ,@body))


;;; `doom-module-context'

(defvar doom-module-context (make-doom-module-context)
  "A `doom-module-context' for the module associated with the current file.

Never set this variable directly, use `with-doom-module'.")

(defmacro with-doom-module (key &rest body)
  "Evaluate BODY with `doom-module-context' informed by KEY."
  (declare (indent 1))
  `(let ((doom-module-context
          (let ((key ,key))
            (if key
                (doom-module-context key)
              (make-doom-module-context)))))
     (doom-log 2 ":context:module: =%s" doom-module-context)
     ,@body))

(defun doom-module-context (key)
  "Return a `doom-module-context' from KEY.

KEY can be a `doom-module-context', `doom-module', or a `doom-module-key' cons
cell."
  (declare (side-effect-free t))
  (or (pcase (type-of key)
        (`doom-module-context key)
        (`doom-module (ignore-errors (doom-module->context key)))
        (`cons (doom-module (car key) (cdr key))))
      (make-doom-module-context :key (doom-module-key key))))

(defun doom-module<-context (context)
  "Return a `doom-module' plist from CONTEXT."
  (declare (side-effect-free t))
  (doom-module-get (doom-module-context-key context)))

(defun doom-module->context (key)
  "Change a `doom-module' into a `doom-module-context'."
  (declare (side-effect-free t))
  (pcase-let
      (((doom-module index path flags group name)
        (if (doom-module-p key)
            key (doom-module-get (doom-module-key key)))))
    (make-doom-module-context
     :index index
     :key (cons group name)
     :path path
     :flags flags)))

(defun doom-module (group name &optional property)
  "Return the `doom-module-context' for any active module by GROUP NAME.

Return its PROPERTY, if specified."
  (declare (side-effect-free t))
  (when-let ((context (get group name)))
    (if property
        (aref
         context
         (or (plist-get
              (eval-when-compile
                (cl-loop with i = 1
                         for info in (cdr (cl-struct-slot-info 'doom-module-context))
                         nconc (list (doom-keyword-intern (symbol-name (car info)))
                                     (prog1 i (cl-incf i)))))
              property)
             (error "Unknown doom-module-context property: %s" property)))
      context)))


;;; `doom-module'

(defun doom-module-key (key)
  "Normalize KEY into a (GROUP . MODULE) tuple representing a Doom module key."
  (declare (pure t) (side-effect-free t))
  (cond ((doom-module-p key)
         (cons (doom-module-group key) (doom-module-name key)))
        ((doom-module-context-p key)
         (doom-module-context-key key))
        ((car-safe key)
         (if (nlistp (cdr-safe key))
             key
           (cons (car key) (cadr key))))
        ((error "Invalid key: %S" key))))

(defun doom-module--has-flag-p (flags wanted-flags)
  "Return t if the list of WANTED-FLAGS satisfies the list of FLAGS."
  (declare (pure t) (side-effect-free error-free))
  (cl-loop with flags = (ensure-list flags)
           for flag in (ensure-list wanted-flags)
           for flagstr = (symbol-name flag)
           if (if (eq ?- (aref flagstr 0))
                  (memq (intern (concat "+" (substring flagstr 1)))
                        flags)
                (not (memq flag flags)))
           return nil
           finally return t))

(defun doom-module--fold-flags (flags)
  "Returns a collapsed list of FLAGS (a list of +/- prefixed symbols).

FLAGS is read in sequence, cancelling out negated flags and removing
duplicates."
  (declare (pure t) (side-effect-free error-free))
  (let (newflags)
    (while flags
      (let* ((flag (car flags))
             (flagstr (symbol-name flag)))
        (when-let ((sym (intern-soft
                         (concat (if (eq ?- (aref flagstr 0)) "+" "-")
                                 (substring flagstr 1)))))
          (setq newflags (delq sym newflags)))
        (cl-pushnew flag newflags :test 'eq))
      (setq flags (cdr flags)))
    (nreverse newflags)))

(defun doom-module-get (key &optional property)
  "Returns the plist for GROUP MODULE. Gets PROPERTY, specifically, if set."
  (declare (side-effect-free t))
  (when-let ((m (gethash key doom-modules)))
    (if property
        (aref
         m (or (plist-get
                (eval-when-compile
                  (cl-loop with i = 1
                           for info in (cdr (cl-struct-slot-info 'doom-module))
                           nconc (list (doom-keyword-intern (symbol-name (car info)))
                                       (prog1 i (cl-incf i)))))
                property)
               (error "Unknown doom-module property: %s" property)))
      m)))

(defun doom-module-active-p (group module &optional flags)
  "Return t if GROUP MODULE is active, and with FLAGS (if given)."
  (declare (side-effect-free t))
  (when-let ((val (doom-module-get (cons group module) (if flags :flags))))
    (or (null flags)
        (doom-module--has-flag-p flags val))))

(defun doom-module-exists-p (group module)
  "Returns t if GROUP MODULE is present in any active source."
  (declare (side-effect-free t))
  (if (doom-module-get group module) t))

(cl-defun doom-module--depth< (keya keyb &optional initorder?)
  "Return t if module with KEY-A comes before another with KEY-B.

If INITORDER? is non-nil, grab the car of the module's :depth, rather than it's
cdr. See `doom-module-put' for details about the :depth property."
  (declare (pure t) (side-effect-free t))
  (let* ((adepth (doom-module-get keya :depth))
         (bdepth (doom-module-get keyb :depth))
         (adepth (if initorder? (car adepth) (cdr adepth)))
         (bdepth (if initorder? (car bdepth) (cdr bdepth))))
    (if (or (null adepth) (null bdepth)
            (= adepth bdepth))
        (< (or (doom-module-get keya :index) 0)
           (or (doom-module-get keyb :index) 0))
      (< adepth bdepth))))

(defun doom-module-list (&optional paths-or-all initorder?)
  "Return a list of (:group . name) module keys in order of their :depth.

PATHS-OR-ALL can either be a non-nil value or a list of directories. If given a
list of directories, return a list of module keys for all modules present
underneath it.  If non-nil, return the same, but search `doom-module-load-path'
(includes :doom and :user). Modules that are enabled are sorted first by their
:depth, followed by disabled modules in lexicographical order (unless a :depth
is specified in their .doommodule).

If INITORDER? is non-nil, sort modules by the CAR of that module's :depth."
  (sort (if paths-or-all
            (delete-dups
             (append (seq-remove #'cdr (doom-module-list nil initorder?))
                     (doom-files-in (if (listp paths-or-all)
                                        paths-or-all
                                      doom-module-load-path)
                                    :map #'doom-module-from-path
                                    :type 'dirs
                                    :mindepth 1
                                    :depth 1)))
          (hash-table-keys doom-modules))
        (doom-rpartial #'doom-module--depth< initorder?)))

(defun doom-module-expand-path (key &optional file)
  "Expands a path to FILE relative to KEY, a cons cell: (GROUP . NAME)

GROUP is a keyword. MODULE is a symbol. FILE is an optional string path.
If the group isn't enabled this returns nil. For finding disabled modules use
`doom-module-locate-path' instead."
  (when-let ((path (doom-module-get key :path)))
    (if file
        (file-name-concat path file)
      path)))

(defun doom-module-locate-path (key &optional file)
  "Searches `doom-module-load-path' to find the path to a module by KEY.

KEY is a cons cell (GROUP . NAME), where GROUP is a keyword (e.g. :lang) and
NAME is a symbol (e.g. \\='python). FILE is a string that will be appended to
the resulting path. If said path doesn't exist, this returns nil, otherwise an
absolute path."
  (let (file-name-handler-alist)
    (if-let* ((path (doom-module-expand-path key file)))
        (if (or (null file)
                (file-exists-p path))
            path)
      (cl-destructuring-bind (group . module) (doom-module-key key)
        (let* ((group (doom-keyword-name group))
               (module (if module (symbol-name module)))
               (path (file-name-concat group module file)))
          (if file
              ;; PERF: locate-file-internal is a little faster for finding files,
              ;;   but its interface for finding directories is clumsy.
              (locate-file-internal path doom-module-load-path '("" ".elc" ".el"))
            (cl-loop for default-directory in doom-module-load-path
                     if (file-exists-p path)
                     return (expand-file-name path))))))))

(defun doom-module-locate-paths (module-list file)
  "Return all existing paths to FILE under each module in MODULE-LIST.

MODULE-LIST is a list of cons cells (GROUP . NAME). See `doom-module-list' for
an example."
  (cl-loop for key in (or module-list (doom-module-list))
           if (doom-module-locate-path key file)
           collect it))

(defun doom-module-from-path (path &optional enabled-only?)
  "Returns a cons cell (GROUP . NAME) derived from PATH (a file path).
If ENABLED-ONLY?, return nil if the containing module isn't enabled."
  (let* ((file-name-handler-alist nil)
         (path (expand-file-name path)))
    (save-match-data
      (cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
             (when-let* ((group (doom-keyword-intern (match-string 1 path)))
                         (name  (intern (match-string 2 path))))
               (and (or (null enabled-only?)
                        (doom-module-active-p group name))
                    (cons group name))))
            ((file-in-directory-p path doom-core-dir)
             (cons :doom nil))
            ((file-in-directory-p path doom-user-dir)
             (cons :user nil))))))

(defun doom-module-load-path (&optional module-load-path)
  "Return a list of file paths to activated modules.

The list is in no particular order and its file paths are absolute. If
MODULE-DIRS is non-nil, include all modules (even disabled ones) available in
those directories."
  (declare (pure t) (side-effect-free t))
  (mapcar #'doom-module-locate-path
          (doom-module-list (or module-load-path doom-module-load-path))))

(put :if     'lisp-indent-function 2)
(put :when   'lisp-indent-function 'defun)
(put :unless 'lisp-indent-function 'defun)

(defmacro doom! (&rest modules)
  "Bootstraps DOOM Emacs and its modules.

If the first item in MODULES doesn't satisfy `keywordp', MODULES is evaluated,
otherwise, MODULES is a variadic-property list (a plist whose key may be
followed by one or more values).

This macro does nothing in interactive sessions, but in noninteractive session
iterates through MODULES, enabling and initializing them. The order of modules
in these blocks dictates their load order (unless given an explicit :depth)."
  `(when noninteractive
     ;; REVIEW: A temporary fix for flycheck until I complete backporting
     ;;   module/profile architecture from v3.0.
     (when (fboundp 'doom-module-mplist-map)
       (doom-module-mplist-map
        #'doom-module--put
        ,@(if (keywordp (car modules))
              (list (list 'quote modules))
            modules)))
     t))

;; DEPRECATED Remove in 3.0
(define-obsolete-function-alias 'featurep! 'modulep! "3.0.0")

(defmacro modulep! (group &optional module &rest flags)
  "Return t if :GROUP MODULE (and +FLAGS) are enabled.

If FLAGS is provided, returns t if GROUP MODULE has all of FLAGS enabled.

  (modulep! :config default +flag)
  (modulep! :config default +flag1 +flag2 +flag3)

GROUP and MODULE may be omitted when this macro is used from a Doom module's
source (except your $DOOMDIR, which is a special module). Like so:

  (modulep! +flag3 +flag1 +flag2)
  (modulep! +flag)

FLAGS can be negated. E.g. This will return non-nil if ':tools lsp' is enabled
without `+eglot':

  (modulep! :tools lsp -eglot)

To interpolate dynamic values, use comma:

  (let ((flag '-eglot))
    (modulep! :tools lsp ,flag))

For more about modules and flags, see `doom!'."
  (if (keywordp group)
      (let ((ctxtform `(get (backquote ,group) (backquote ,module))))
        (if flags
            `(when-let* ((ctxt ,ctxtform))
               (doom-module--has-flag-p
                (doom-module-context-flags ctxt)
                (backquote ,flags)))
          `(and ,ctxtform t)))
    (let ((flags (delq nil (cons group (cons module flags)))))
      (if (doom-module-context-index doom-module-context)
          `(doom-module--has-flag-p
            ',(doom-module-context-flags doom-module-context)
            (backquote ,flags))
        `(let ((file (file!)))
           (if-let* ((module (doom-module-from-path file)))
               (doom-module--has-flag-p
                (doom-module (car module) (cdr module) :flags)
                (backquote ,flags))
             (error "(modulep! %s) couldn't resolve current module from %s"
                    (backquote ,flags) (abbreviate-file-name file))))))))


;;; `doom-package'

(cl-defmacro package!
    (name &rest plist &key built-in recipe ignore _type _pin _disable)
  "Declares a package and how to install it (if applicable).

This macro is declarative and does not load nor install packages. It is used to
populate `doom-packages' with metadata about the packages Doom needs to keep
track of.

Only use this macro in a module's packages.el file.

Accepts the following properties:

 :type core|local|built-in|virtual
   Specifies what kind of package this is. Can be a symbol or a list thereof.
     `core' = this is a protected package and cannot be disabled!
     `local' = this package is being modified in-place. This package's repo is
       unshallowed and will be skipped when you update packages.
     `built-in' = this package is already built-in (otherwise, will be
       installed)
     `virtual' = this package is not tracked by Doom's package manager. It won't
       be installed or uninstalled. Use this to pin 2nd order dependencies.
 :recipe RECIPE
   Specifies a straight.el recipe to allow you to acquire packages from external
   sources. See https://github.com/radian-software/straight.el#the-recipe-format
   for details on this recipe.
 :disable BOOL
   Do not install or update this package AND disable all of its `use-package!'
   and `after!' blocks.
 :ignore FORM
   Do not install this package.
 :pin STR|nil
   Pin this package to commit hash STR. Setting this to nil will unpin this
   package if previously pinned.
 :built-in BOOL|'prefer
   Same as :ignore if the package is a built-in Emacs package. This is more to
   inform help commands like `doom/help-packages' that this is a built-in
   package. If set to 'prefer, the package will not be installed if it is
   already provided by Emacs.

Returns t if package is successfully registered, and nil if it was disabled
elsewhere."
  (declare (indent defun))
  (when (and recipe (keywordp (car-safe recipe)))
    (cl-callf plist-put plist :recipe `(quote ,recipe)))
  ;; :built-in t is basically an alias for :ignore (locate-library NAME)
  (when built-in
    (when (and (not ignore)
               (equal built-in '(quote prefer)))
      (setq built-in `(locate-library ,(symbol-name name) nil (get 'load-path 'initial-value))))
    (cl-callf map-delete plist :built-in)
    (cl-callf plist-put plist :ignore built-in))
  `(let* ((name ',name)
          (plist (cdr (assq name doom-packages)))
          (dir (dir!))
          (module (doom-module-from-path dir)))
     (unless (doom-context-p 'package)
       (signal 'doom-module-error
               (list module "package! can only be used in packages.el files")))
     ;; Record what module this declaration was found in
     (let ((module-list (plist-get plist :modules)))
       (unless (member module module-list)
         (cl-callf plist-put plist :modules
                   (append module-list
                           (list module)
                           (when (file-in-directory-p dir doom-user-dir)
                             '((:user . modules)))
                           nil))))
     ;; Merge given plist with pre-existing one
     (cl-loop for (key value) on (list ,@plist) by 'cddr
              when (or (eq key :pin) value)
              do (cl-callf plist-put plist key value))
     ;; Some basic key validation; throws an error on invalid properties
     (condition-case e
         (when-let (recipe (plist-get plist :recipe))
           (cl-destructuring-bind
               (&key local-repo _files _flavor _build _pre-build _post-build
                     _includes _type _repo _host _branch _protocol _remote
                     _nonrecursive _fork _depth _source _inherit)
               recipe
             ;; Expand :local-repo from current directory
             (when local-repo
               (cl-callf plist-put plist :recipe
                         (plist-put recipe :local-repo
                                    (let ((local-path (expand-file-name local-repo dir)))
                                      (if (file-directory-p local-path)
                                          local-path
                                        local-repo)))))))
       (error
        (signal 'doom-package-error
                (cons ,(symbol-name name)
                      (error-message-string e)))))
     ;; These are the only side-effects of this macro!
     (setf (alist-get name doom-packages) plist)
     (if (plist-get plist :disable)
         (add-to-list 'doom-disabled-packages name)
       (with-no-warnings
         (cons name plist)))))

;; DEPRECATED: Will be replaced with new `packages!' macro in v3.0
(defmacro disable-packages! (&rest packages)
  "A convenience macro for disabling packages in bulk.
Only use this macro in a module's (or your private) packages.el file."
  (macroexp-progn
   (mapcar (lambda (p) `(package! ,p :disable t))
           packages)))

;; DEPRECATED: Will be replaced with new `packages!' macro in v3.0
(defmacro unpin! (&rest targets)
  "Unpin packages in TARGETS.

This unpins packages, so that `doom upgrade' or `doom sync -u' will update them
to the latest commit available. Some examples:

- To disable pinning wholesale: (unpin! t)
- To unpin individual packages: (unpin! packageA packageB ...)
- To unpin all packages in a group of modules: (unpin! :lang :tools ...)
- To unpin packages in individual modules:
    (unpin! (:lang python javascript) (:tools docker))

Or any combination of the above.

This macro should only be used from the user's private packages.el. No module
should use it!"
  (if (memq t targets)
      `(mapc (doom-rpartial #'doom-package-set :unpin t)
             (mapcar #'car doom-packages))
    (macroexp-progn
     (mapcar
      (lambda (target)
        (when target
          `(doom-package-set ',target :unpin t)))
      (cl-loop for target in targets
               if (or (keywordp target) (listp target))
               append
               (cl-loop with (category . modules) = (ensure-list target)
                        for (name . plist) in doom-packages
                        for pkg-modules = (plist-get plist :modules)
                        if (and (assq category pkg-modules)
                                (or (null modules)
                                    (cl-loop for module in modules
                                             if (member (cons category module) pkg-modules)
                                             return t))
                                name)
                        collect it)
               else if (symbolp target)
               collect target)))))


;;; `doom-profile'

(defun doom-profile-key (profile &optional default?)
  "Normalize PROFILE into a (NAME . REF) doom-profile key.

PROFILE can be a `doom-profile', a profile id (i.e. a string in the NAME@REF
format), or a (NAME . REF) cons cell.

If DEFAULT? is non-nil, an unspecified CAR/CDR will fall bakc to (_default .
0)."
  (declare (pure t) (side-effect-free t))
  (let ((default-name (if default? "_default"))
        (default-ref  (if default? "0")))
    (cond ((eq profile t) (cons default-name default-ref))
          ;; ((doom-profile-p profile)
          ;;  (cons (or (doom-profile-name profile) default-name)
          ;;        (or (doom-profile-ref profile)  default-ref)))
          ((stringp profile)
           (save-match-data
             (let (case-fold-search)
               (if (string-match "^\\([^@]+\\)@\\(.+\\)$" profile)
                   (cons (match-string 1 profile)
                         (match-string 2 profile))
                 (cons profile default-ref)))))
          ((and (consp profile) (nlistp (cdr profile)))
           (cons (or (car profile) default-name)
                 (or (cdr profile) default-ref)))
          ((and (null profile) default?)
           (cons default-name default-ref))
          ((signal 'wrong-type-argument
                   (list "Expected PROFILE to be a string, cons cell, or `doom-profile'"
                         (type-of profile) profile))))))

(defun doom-profile-init-file (profile)
  "Return the init file for PROFILE."
  (declare (side-effect-free t))
  (cl-destructuring-bind (name . ref)
      (if profile
          (doom-profile-key profile t)
        (cons nil nil))
    (file-name-concat doom-data-dir name "@" ref
                      (format "init.%d.%d.el"
                              emacs-major-version
                              emacs-minor-version))))

(defun doom-profile-get (profile-name &optional property null-value)
  "Return PROFILE-NAME's PROFILE, otherwise its PROPERTY, otherwise NULL-VALUE."
  (when (stringp profile-name)
    (setq profile-name (intern profile-name)))
  (if-let* ((profile (assq profile-name (doom-profiles))))
      (if property
          (if-let* ((propval (assq property (cdr profile))))
              (cdr propval)
            null-value)
        profile)
    null-value))

(defun doom-profile->id (profile)
  "Return a NAME@VERSION id string from profile cons cell (NAME . VERSION)."
  (cl-check-type profile cons)
  (cl-destructuring-bind (name . ref) (doom-profile-key profile)
    (format "%s@%s" name ref)))

(provide 'doom-lib)
;;; doom-lib.el ends here
