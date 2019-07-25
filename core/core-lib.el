;;; core-lib.el -*- lexical-binding: t; -*-

(let ((load-path doom--initial-load-path))
  (require 'subr-x)
  (require 'cl-lib))

;; Polyfills
(unless EMACS26+
  (with-no-warnings
    ;; `kill-current-buffer' was introduced in Emacs 26
    (defalias 'kill-current-buffer #'kill-this-buffer)
    ;; if-let and when-let were moved to (if|when)-let* in Emacs 26+ so we alias
    ;; them for 25 users.
    (defalias 'if-let* #'if-let)
    (defalias 'when-let* #'when-let)

    (defun alist-get (key alist &optional default remove testfn)
      "Return the value associated with KEY in ALIST.
If KEY is not found in ALIST, return DEFAULT.
Use TESTFN to lookup in the alist if non-nil.  Otherwise, use `assq'.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT."
      (ignore remove) ;;Silence byte-compiler.
      (let ((x (if (not testfn)
                   (assq key alist)
                 ;; In Emacs<26, `assoc' has no testfn arg, so we have to
                 ;; implement it ourselves
                 (if testfn
                     (cl-loop for entry in alist
                              if (funcall testfn key entry)
                              return entry)
                   (assoc key alist)))))
        (if x (cdr x) default)))))


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

This is used by `file-exists-p!' and `project-file-exists-p!'."
  (declare (pure t) (side-effect-free t))
  (let ((exists-fn (if (fboundp 'projectile-file-exists-p)
                       #'projectile-file-exists-p
                     #'file-exists-p)))
    (if (and (listp spec)
             (memq (car spec) '(or and)))
        (cons (car spec)
              (mapcar (doom-rpartial #'doom--resolve-path-forms directory)
                      (cdr spec)))
      (let ((filevar (make-symbol "file")))
        `(let* ((file-name-handler-alist nil)
                (,filevar ,spec))
           (and ,(if directory
                     `(let ((default-directory ,directory))
                        (,exists-fn ,filevar))
                   (list exists-fn filevar))
                ,filevar))))))

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
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defmacro doom-log (format-string &rest args)
  "Log to *Messages* if `doom-debug-mode' is on.
Does not interrupt the minibuffer if it is in use, but still logs to *Messages*.
Accepts the same arguments as `message'."
  `(when doom-debug-mode
     (let ((inhibit-message (active-minibuffer-window)))
       (message
        ,(concat (propertize "DOOM " 'face 'font-lock-comment-face)
                 (when (bound-and-true-p doom--current-module)
                   (propertize
                    (format "[%s/%s] "
                            (doom-keyword-name (car doom--current-module))
                            (cdr doom--current-module))
                    'face 'warning))
                 format-string)
        ,@args))))

(defalias 'doom-partial #'apply-partially)

(defun doom-rpartial (fn &rest args)
  "Return a function that is a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))


;;
;;; Sugars

(defmacro λ! (&rest body)
  "Expands to (lambda () (interactive) ,@body)."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))
(defalias 'lambda! 'λ!)

(defun λ!! (command &optional arg)
  "Expands to a command that interactively calls COMMAND with prefix ARG."
  (declare (doc-string 1))
  (lambda () (interactive)
     (let ((current-prefix-arg arg))
       (call-interactively command))))
(defalias 'lambda!! 'λ!!)

(defun file! ()
  "Return the emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
        (load-in-progress load-file-name)
        ((stringp (car-safe current-load-list))
         (car current-load-list))
        (buffer-file-name)))

(defun dir! ()
  "Returns the directory of the emacs lisp file this macro is called from."
  (when-let (path (file!))
    (directory-file-name (file-name-directory path))))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values))
       (cl-pushnew ,var ,place))))

(defmacro pushmany! (place &rest values)
  "Push VALUES sequentually into PLACE.
This is a variadic `push'."
  (let ((var (make-symbol "result")))
    `(dolist (,var ,values)
       (push ,var ,place))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro nconcq! (sym &rest lists)
  "Append LISTS to SYM by altering them in place."
  `(setq ,sym (nconc ,sym ,@lists)))

(defmacro delq! (elt list &optional fetcher)
  "Delete ELT from LIST in-place."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        (fn (intern (format "doom--transient-%s-h" (sxhash hook-or-function)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
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
  3. The function(s) to be added: this can be one function, a list thereof, a
     list of `defun's, or body forms (implicitly wrapped in a closure).

\(fn [:append :local] HOOKS FUNCTIONS)"
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let* ((defun-forms nil)
           (hooks (doom--resolve-hook-forms (pop args)))
           (funcs
            (let ((val (car args)))
              (if (memq (car-safe val) '(quote function))
                  (if (cdr-safe (cadr val))
                      (cadr val)
                    (list (cadr val)))
                (or (and (eq (car-safe val) 'defun)
                         (cl-loop for arg in args
                                  if (not (eq (car-safe arg) 'defun))
                                  return nil
                                  else
                                  collect (cadr arg)
                                  and do (push arg defun-forms)))
                    (list args)))))
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
      (macroexp-progn
       (append (nreverse defun-forms)
               (if append-p (nreverse forms) forms))))))

(defmacro remove-hook! (&rest args)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn [:append :local] HOOKS FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! :remove ,@args))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

  (setq-hook! 'markdown-mode-hook
    line-spacing 2
    fill-column 80)

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (doom--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(remove-hook ',hook #',fn) ; ensure set order
            collect `(add-hook ',hook #',fn 'append))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn) in (doom--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

(defmacro file-exists-p! (files &optional directory)
  "Returns non-nil if the FILES in DIRECTORY all exist.

DIRECTORY is a path; defaults to `default-directory'.

Returns the last file found to meet the rules set by FILES, which can be a
single file or nested compound statement of `and' and `or' statements."
  `(let ((p ,(doom--resolve-path-forms files directory)))
     (and p (expand-file-name p ,directory))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name').

FILENAME is either a file path string or a form that should evaluate to such a
string at run time. PATH is where to look for the file (a string representing a
directory path). If omitted, the lookup is relative to either `load-file-name',
`byte-compile-current-file' or `buffer-file-name' (checked in that order).

If NOERROR is non-nil, don't throw an error if the file doesn't exist."
  (unless path
    (setq path (or (dir!)
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

(defmacro defer-feature! (feature &optional mode)
  "Pretend FEATURE hasn't been loaded yet, until FEATURE-hook is triggered.

Some packages (like `elisp-mode' and `lisp-mode') are loaded immediately at
startup, which will prematurely trigger `after!' (and `with-eval-after-load')
blocks. To get around this we make Emacs believe FEATURE hasn't been loaded yet,
then wait until FEATURE-hook (or MODE-hook, if MODE is provided) is triggered to
reverse this and trigger `after!' blocks at a more reasonable time."
  (let ((advice-fn (intern (format "doom--defer-feature-%s-a" feature)))
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


;;
;;; Definers

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called NAME and add it to PLACES.

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
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       ,(when where-alist
          `(dolist (targets (list ,@(nreverse where-alist)))
             (dolist (target (cdr targets))
               (advice-add target (car targets) #',symbol)))))))

(provide 'core-lib)
;;; core-lib.el ends here
