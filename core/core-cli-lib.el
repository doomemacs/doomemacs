;;; core/core-cli-lib.el --- API+DSL for Doom's CLI framework -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'seq)
(require 'map)


;;
;;; Variables

(defvar doom-cli-load-path
  (ignore-errors (split-string (getenv "DOOMPATH") path-separator))
  "A list of paths to search for autoloaded CLIs on.")

(defvar doom-cli-argument-types
  '(&args
    &cli
    &context
    &flags
    &multiple
    &optional
    &rest
    &required
    &input
    &whole)
  "A list of auxiliary keywords allowed in `defcli!'s arglist.

See `defcli!' for documentation on them.")

(defvar doom-cli-option-types
  '((&flag  . &flags)
    (&multi . &multiple))
  "An alist of auxiliary keywords permitted in option specs in `defcli!'.

They serve as shorter, inline aliases for `doom-cli-argument-types'.

See `defcli!' for documentation on them.")

(defvar doom-cli-option-generators
  '((&flags    . doom-cli--make-option-flag)
    (&multiple . doom-cli--make-option-multi)
    (&required . doom-cli--make-option-generic)
    (&optional . doom-cli--make-option-generic))
  "An alist of `doom-cli-option' factories for argument types.

Types that

See argument types in `doom-cli-argument-types', and `defcli!' for usage.")

(defvar doom-cli-option-arg-types
  `((dir    :test file-directory-p
            :read doom-path
            :error "Directory does not exist"
            :zshcomp "_dirs")
    (file   :test file-exists-p
            :read doom-path
            :error "File does not exist"
            :zshcomp "_files")
    (path   :read expand-file-name :zshcomp "_files")
    (form   :read read)
    (regexp :test ,(doom-rpartial #'string-match-p ""))
    (int    :test "^[0-9]+$"
            :read string-to-number
            :error "Not an integer")
    (num    :test "^[0-9]+\\(\\.[0-9]+\\)?$"
            :read string-to-number
            :error "Not a valid number or float")
    (float  :test "^[0-9]+\\(\\.[0-9]+\\)$"
            :read string-to-number
            :error "Not a float")
    (bool   :test "^y\\(?:es\\)?\\|no?\\|on\\|off\\|t\\(?:rue\\)?\\|false\\|[01]\\|$"
            :read ,(lambda (x)
                     (pcase x
                       ((or "y" "yes" "t" "true" "1" "on") :yes)
                       ((or "n" "no"  "nil" "false" "0" "off") :no)))
            :error "Not a valid boolean, should be blank or one of: yes, no, y, n, true, false, on, off"
            :zshcomp "(y n yes no true false on off 1 0)")
    ;; TODO Implement these implicit types
    ;; (date ...)
    ;; (time ...)
    ;; (duration ...)
    ;; (size ...)
    )
  "A list of implicit option argument datatypes and their rules.

Recognizies the following properies:

  :test FN
    Predicate function to determine if a value is valid.
  :read FN
    A transformer that converts the string argument to a desired format.
  :error STR
    The message to display if a value fails :test.")

(defvar doom-cli-exit-commands
  '(;; (:editor  . doom-cli--exit-editor)
    ;; (:emacs   . doom-cli--exit-emacs)
    (:pager   . doom-cli--exit-pager)
    (:pager?  . doom-cli--exit-pager-maybe)
    (:restart . doom-cli--exit-restart))
  "An alist of commands that `doom-cli--exit' recognizes.")

(defvar doom-cli-pager (getenv "DOOMPAGER")
  "The PAGER command to use.

If nil, falls back to less.")

(defvar doom-cli-pager-ratio 1.0
  "If output exceeds TTY height times this ratio, the pager is invoked.

Only applies if (exit! :pager) or (exit! :pager?) are called.")

(defvar doom-cli-log-file-format (expand-file-name "logs/cli.%s.%s.%s" doom-local-dir)
  "Where to write any output/log file to.

Must have two arguments, one for session id and the other for log type.")

(defvar doom-cli-log-retain 12
  "Number of each log type to retain.")

(defvar doom-cli-backtrace-depth 12
  "How many frames of the backtrace to display in stdout.")

(defvar doom-cli-straight-error-lines 16
  "How many lines of straight.el errors to display in stdout.")

(defvar doom-cli-benchmark-threshold 5
  "How much execution time (in seconds) before benchmark is shown.

If set to nil, only display benchmark if a CLI explicitly requested with a
non-nil :benchmark property.")

(defvar doom-cli--context nil)
(defvar doom-cli--exit-code 255)
(defvar doom-cli--plist nil)
(defvar doom-cli--table (make-hash-table :test 'equal))


;;
;;; Hooks

(defvar doom-cli-create-context-functions ()
  "A hook executed once a new context has been generated.

Called by `doom-cli-context-parse' and `doom-cli-context-restore', once a
`doom-cli-context' is fully populated and ready to be executed (but before it
has).

Hooks are run with one argument: the newly created context.")

(defvar doom-cli-before-run-functions ()
  "Hooks run before `doom-cli-run' executes the command.

Runs with a single argument: the active context (a `doom-cli-context' struct).")

(defvar doom-cli-after-run-functions ()
  "Hooks run after `doom-cli-run' has executed the command.

Runs with two arguments: the active context (a `doom-cli-context' struct) and
the return value of the executed CLI.")


;;
;;; Errors

(define-error 'doom-cli-error "There was an unexpected error" 'doom-error)
(define-error 'doom-cli-definition-error "Invalid CLI definition" 'doom-cli-error)
(define-error 'doom-cli-autoload-error "Failed to autoload deferred command" 'doom-cli-error)
(define-error 'doom-cli-invalid-prefix-error "Prefix has no defined commands" 'doom-cli-error)
(define-error 'doom-cli-command-not-found-error "Could not find that command" 'doom-cli-error)
(define-error 'doom-cli-wrong-number-of-arguments-error "Wrong number of CLI arguments" 'doom-cli-error)
(define-error 'doom-cli-unrecognized-option-error "Not a recognized option" 'doom-cli-error)
(define-error 'doom-cli-invalid-option-error "Invalid option value" 'doom-cli-error)


;;
;;; `doom-cli'

(cl-defstruct (doom-cli (:constructor doom-cli-create)
                        (:copier nil))
  "An executable CLI command."
  (command nil :read-only t)
  type
  docs
  autoload
  alias
  options
  arguments
  plist
  fn)

(defun doom-cli-execute (cli bindings)
  "Execute CLI with BINDINGS (an alist).

BINDINGS is an alist of (SYMBOL . VALUE) to bind lexically during CLI's
execution. Can be generated from a `doom-cli-context' with
`doom-cli--bindings'."
  (doom-log "doom-cli-execute: %s %s" (doom-cli-key cli) bindings)
  (funcall (doom-cli-fn cli) cli bindings))

(defun doom-cli-key (cli)
  "Return CLI's (type . command), used as a table key or unique identifier."
  (let ((command (doom-cli-command cli)))
    (if-let (type (doom-cli-type cli))
        (cons type command)
      command)))

(defun doom-cli-command-normalize (command &optional plist)
  "Ensure that COMMAND is properly formatted.

This means that all non-keywords are strings, any prefixes provided by PLIST are
prepended, and the keyword is in front."
  (let* ((command (doom-enlist command))
         (prefix  (plist-get plist :prefix))
         (prefix  (if prefix (doom-cli-command-normalize
                              prefix (append `(:prefix nil) plist))))
         (command (append prefix command))
         (type    (cl-find-if #'keywordp (remq :root command) :from-end t))
         (command (seq-subseq
                   command (or (cl-position :root command :from-end t)
                               0))))
    (when (or command prefix)
      (cl-loop with map = (fn! (if (or (stringp %) (keywordp %)) % (prin1-to-string %)))
               for c in (delq nil (cons type (seq-remove #'keywordp command)))
               if (listp c)
               collect (mapcar map c)
               else collect (funcall map c)))))

(defun doom-cli-command-string (command)
  "Return a joined string representation of normalized COMMAND.

COMMAND should either be a command list (e.g. '(doom foo bar)) or a `doom-cli'
struct."
  (mapconcat (doom-partial #'format "%s")
             (doom-cli--command command)
             " "))

(defun doom-cli-get (command &optional noresolve? noload?)
  "Return CLI at COMMAND.

Will autoload COMMAND if it was deferred with `defautoload!'.

If NORESOLVE?, don't follow aliases."
  (when-let* ((command (doom-cli--command command))
              (cli (gethash command doom-cli--table))
              (cli (if noload? cli (doom-cli-load cli))))
    (if noresolve?
        cli
      (let (path)
        (while (setq path (ignore-errors (doom-cli-alias cli)))
          (setq cli (doom-cli-get path t noload?)))
        (unless cli
          (signal 'doom-cli-command-not-found-error (or path command)))
        cli))))

(defun doom-cli-path (cli &optional noload?)
  "Return a list of `doom-cli's encountered while following CLI's aliases.

If NOLOAD? is non-nil, don't autoload deferred CLIs (see `doom-cli-get')."
  (when cli
    (cons
     cli (let (alias paths)
           (while (setq alias (ignore-errors (doom-cli-alias cli)))
             (and (setq cli (doom-cli-get alias t noload?))
                  (push cli paths)))
           (nreverse paths)))))

(defun doom-cli-find (command &optional norecursive)
  "Find all CLIs assocated with COMMAND, excluding partials if NORECURSIVE.

COMMAND can be a command path (list of strings), a `doom-cli' struct, or a
`doom-cli-context' struct.

Returned in the order they will execute. Includes pseudo CLIs."
  (let* ((command (doom-cli--command command))
         (paths (nreverse (doom-cli--command-expand command t)))
         results clis)
    (push '(:after) results)
    (dolist (path paths)
      (push (cons :after path) results))
    (push command results)
    (dolist (path (nreverse paths))
      (push (cons :before path) results))
    (push '(:before) results)
    (dolist (result results (nreverse clis))
      (when-let (cli (doom-cli-get result t))
        (cl-pushnew cli clis
                    :test #'equal
                    :key #'doom-cli-key)))))

(defun doom-cli-prop (cli prop &optional null-value)
  "Returns a PROPerty of CLI's plist, or NULL-VALUE if it doesn't exist."
  (let ((plist (doom-cli-plist cli)))
    (if (plist-member plist prop)
        (plist-get plist prop)
      null-value)))

(cl-defun doom-cli-subcommands (command &optional (depth 9999) &key tree? all? predicate?)
  "Return a list of subcommands, DEPTH levels deep, below COMMAND.

  If DEPTH is non-nil, list *all* subcommands, recursively. Otherwise it expects
an integer.
  If TREE?, return commands in a tree structure.
  If ALL?, include hidden commands (like aliases)."
  (when (or (null depth) (> depth 0))
    (catch :predicate
      (let* ((command (doom-cli--command command t))
             (prefixlen (length command))
             results)
        (dolist (cli (hash-table-values doom-cli--table))
          (let ((clicmd (doom-cli-command cli)))
            (when (and (not (doom-cli-type cli))
                       (= (length clicmd) (1+ prefixlen))
                       (equal command (seq-take clicmd prefixlen))
                       (or all? (not (doom-cli-prop cli :hide))))
              (when predicate?
                (throw :predicate t))
              (let* ((car (if tree? (car (last clicmd)) clicmd))
                     (cdr (doom-cli-subcommands
                           clicmd (if depth (1- depth))
                           :tree? tree?
                           :all?  all?)))
                (if tree?
                    (push (if cdr (cons car cdr) car) results)
                  (cl-callf nconc results (cons car cdr)))))))
        (if tree?
            (nreverse results)
          results)))))

(defun doom-cli-aliases (cli)
  "Return all known `doom-cli's that are aliased to CLI.

This cannot see autoloaded CLIs. Use `doom-cli-load' or `doom-cli-load-all'
to reach them."
  (cl-loop for rcli in (hash-table-values doom-cli--table)
           if (equal (doom-cli-key (doom-cli-get rcli))
                     (doom-cli-key rcli))
           collect rcli))

(defun doom-cli-short-docs (cli)
  "Return the first line of CLI's documentation.

Return nil if CLI (a `doom-cli') has no explicit documentation."
  (ignore-errors (cdr (assoc "SUMMARY" (doom-cli-docs cli)))))

(defun doom-cli--bindings (cli context &optional seen)
  "Return a CLI with a value alist in a cons cell."
  (let* ((optspec (doom-cli-options cli))
         (argspec (doom-cli-arguments cli))
         alist)
    ;; Ensure all symbols are defined
    (dolist (opt optspec)
      (setf (alist-get (doom-cli-option-symbol opt) alist)
            (doom-cli-option-default opt)))
    (dolist (syms argspec)
      (dolist (sym (cdr syms))
        (setf (alist-get sym alist) nil)))
    ;; Populate options
    (let ((options (doom-cli-context-options context)))
      (dolist (opt optspec)
        (when-let (option (cl-loop for flag in (doom-cli-option-switches opt)
                                   if (cdr (assoc flag options))
                                   return (cons flag it)))
          (unless (member (car option) seen)
            (setf (alist-get (doom-cli-option-symbol opt) alist)
                  (cdr option))
            (push (car option) seen)))))
    ;; Populate arguments
    (let* ((arglist  (doom-cli-context-arguments context))
           (rest     (copy-sequence (map-elt arglist (doom-cli-command cli))))
           (args     (copy-sequence (alist-get t arglist)))
           (argc     (length args))
           (required (alist-get '&required argspec))
           (optional (alist-get '&optional argspec))
           (spec     (append required optional))
           (min      (length required))
           (max      (if (or (assq '&args argspec)
                             (assq '&rest argspec))
                         most-positive-fixnum
                       (length spec))))
      (when (or (< argc min)
                (> argc max))
        (signal 'doom-cli-wrong-number-of-arguments-error
                (list (doom-cli-key cli) nil args min max)))
      (dolist (sym spec)
        (setf (alist-get sym alist) (if args (pop args))))
      (dolist (type `((&args    . ,args)
                      (&cli     . ,cli)
                      (&context . ,context)
                      (&input
                       . ,(if (doom-cli-context-pipe-p context :in t)
                              (with-current-buffer (doom-cli-context-stdin context)
                                (buffer-string))))
                      (&rest    . ,rest)
                      (&whole   . ,(doom-cli-context-whole context))))
        (when-let (var (car (alist-get (car type) argspec)))
          (setf (alist-get var alist) (cdr type)))))
    alist))

(defun doom-cli--command (target &optional notype?)
  "Fetch the normalized command from TARGET.

If NOTYPE? is non-nil, omit any leading keywords from the command.

TARGET can be a `doom-cli', `doom-cli-context', or a command list."
  (cond ((doom-cli-p target)
         (if notype?
             (doom-cli-command target)
           (doom-cli-key target)))
        ((doom-cli-context-p target)
         (doom-cli-context-command target))
        ((and target (not (listp target)))
         (signal 'wrong-type-argument
                 (list '(doom-cli-p doom-cli-context-p listp) target)))
        ((let ((target (doom-cli-command-normalize target)))
           (if (and notype? (keywordp (car target)))
               (cdr target)
             target)))))

(defun doom-cli--command-expand (commandspec &optional recursive?)
  "Expand COMMANDSPEC into a list of commands.

If RECURSIVE, includes breadcrumbs leading up to COMMANDSPEC."
  (funcall (if recursive?
               #'identity
             (fn! (cl-loop with cmdlen = (length (car %))
                           for command in %
                           while (= (length command) cmdlen)
                           collect command)))
           (seq-reduce (lambda (init next)
                         (nconc (cl-loop with firstlen = (length (car init))
                                         for seg in (doom-enlist next)
                                         nconc
                                         (cl-loop for command in init
                                                  while (= (length command) firstlen)
                                                  collect (append command (list seg))))
                                init))
                       (cdr commandspec)
                       `(,@(mapcar #'list (doom-enlist (car commandspec)))))))

(defun doom-cli--parse-docs (docs)
  (when (and (stringp docs)
             (not (equal docs "TODO")))
    (let ((re "^\\([A-Z0-9 _-]+\\):\n") sections)
      (with-temp-buffer
        (save-excursion
          (insert "__DOOMDOCS__:\n")
          (insert docs))
        (while (re-search-forward re nil t)
          (push (cons (match-string 1)
                      (let ((buffer (current-buffer))
                            (beg (match-end 0))
                            (end (save-excursion
                                   (if (re-search-forward re nil t)
                                       (1- (match-beginning 0))
                                     (point-max)))))
                        (with-temp-buffer
                          (insert-buffer-substring buffer beg end)
                          (goto-char (point-min))
                          (indent-rigidly (point-min) (point-max) (- (skip-chars-forward " ")))
                          (string-trim-right (buffer-string)))))
                sections)))
      (let ((lines (split-string (cdr (assoc "__DOOMDOCS__" sections)) "\n"))
            (sections (assoc-delete-all "__DOOMDOCS__" sections)))
        `(("SUMMARY" . ,(car lines))
          ("MAIN"    . ,(string-trim (string-join (cdr lines) "\n")))
          ,@(nreverse sections))))))


;;
;;; `doom-cli-option'

(cl-defstruct (doom-cli-option (:constructor doom-cli-option-create)
                               (:copier nil))
  "A switch specification dictating the characteristics of a recognized option."
  (symbol nil :read-only t)
  docs
  multiple-p
  flag-p
  switches
  arguments
  default)

(defun doom-cli-option-validate (option &rest values)
  "Test if OPTION will accept VALUES, and conforms them if necessary.

OPTION is a `doom-cli-option' struct. VALUES can be any arbitrary values.
Returns VALUES once mapped through their respective reader (as dictated by
`doom-cli-option-arg-types').

Throws `doom-cli-invalid-option-error' for illegal values."
  (let ((args (doom-cli-option-arguments option))
        (values (copy-sequence values)))
    (dotimes (i (length args) values)
      (let ((value (nth i values))
            (types (ensure-list (nth i args)))
            errors)
        (catch 'done
          (dolist (type (ensure-list (nth i args)))
            (pcase-let
                (((map :test :read :error)
                  (if (or (symbolp type)
                          (and (stringp type)
                               (string-match-p "^[A-Z0-9-_]+$" type)))
                      (cdr (assq (if (symbolp type) type (intern type))
                                 doom-cli-option-arg-types))
                    (list 'str :test #'stringp))))
              (condition-case-unless-debug e
                  (or (and (or (null test)
                               (if (stringp test)
                                   (and (string-match-p test value) t)
                                 (funcall test value)))
                           (or (null read)
                               (setf (nth i values) (funcall read value)))
                           (throw 'done t))
                      (push error errors))
                ((invalid-regexp invalid-read-syntax)
                 (push (error-message-string e) errors)))))
          (signal 'doom-cli-invalid-option-error
                  (list types option value errors)))))))

(defun doom-cli--read-option-switches (optspec)
  (delq
   nil (cl-loop for spec in optspec
                if (and (stringp spec)
                        (string-match-p "^-\\(?:-[a-zA-Z0-9]\\|[^-]$\\)" spec))
                collect spec)))

(defun doom-cli--read-option-args (argspec)
  (delq
   nil (cl-loop for spec in argspec
                if (or (and (stringp spec)
                            (not (string-match-p "^-\\(?:-[a-zA-Z0-9]\\|[^-]$\\)" spec)))
                       (keywordp spec))
                collect spec
                else if (symbolp spec)
                collect spec)))

(defun doom-cli--make-option-generic (symbol spec &optional docs)
  (doom-cli-option-create
   :symbol symbol
   :docs docs
   :switches  (doom-cli--read-option-switches spec)
   :arguments (doom-cli--read-option-args spec)))

(defun doom-cli--make-option-flag (symbol spec &optional docs)
  (let ((switches (doom-cli--read-option-switches spec))
        (args     (doom-cli--read-option-args spec)))
    (when (and args
               (not (or (memq :yes args)
                        (memq :no args))))
      (signal 'doom-cli-definition-error
              (cons "Argument type %s cannot accept arguments for: %s"
                    '&flag (mapconcat #'symbol-name spec ", "))))
    (doom-cli-option-create
     :symbol symbol
     :docs docs
     :flag-p t
     :switches switches
     :default (car args))))

(defun doom-cli--make-option-multi (symbol spec &optional docs)
  (doom-cli-option-create
   :symbol symbol
   :docs docs
   :multiple-p t
   :switches (doom-cli--read-option-switches spec)
   :arguments (doom-cli--read-option-args spec)))


;;
;;; `doom-cli-context'

(cl-defstruct (doom-cli-context (:constructor doom-cli-context-create)
                                (:copier doom-cli-context-copy))
  "A CLI context, containing all state pertinent to the current session."
  (init-time before-init-time) ; When this context was created
  ;; A session-specific ID of the current context (defaults to number
  (pid (if-let (pid (getenv "__DOOMPID"))
           (string-to-number pid)
         (emacs-pid)))
  ;; Number of Emacs processes this context has been processed through
  (step (if-let (step (getenv "__DOOMSTEP"))
            (string-to-number step)
          -1))
  ;; The geometry of the terminal window.
  (geometry (save-match-data
              (when-let* ((geom (getenv "__DOOMGEOM"))
                          ((string-match "^\\([0-9]+\\)x\\([0-9]+\\)$" geom)))
                (cons (string-to-number (match-string 1 geom))
                      (string-to-number (match-string 2 geom))))))
  ;; Whether the script is being piped into or out of
  (pipes (cl-loop for (env . scope) in `((,(getenv "__DOOMGPIPE") . global)
                                         (,(getenv "__DOOMPIPE")  . local))
                  if (stringp env)
                  for pipes = (string-to-list env)
                  nconc `(,@(if (memq ?0 pipes) `((:in  . ,scope)))
                          ,@(if (memq ?1 pipes) `((:out . ,scope)))))
         :skip t)
  ;; If non-nil, suppress prompts and auto-accept their consequences.
  suppress-prompts-p
  (prefix "@")  ; The basename of the script creating this context
  meta-p        ; Whether or not this is a help/meta request
  error         ;
  command       ; The full command that led to this context
  path          ; Breadcrumb list of resolved commands so far
  whole         ; Unfiltered and unprocessed list of arguments
  options       ; An alist of (flags . value)
  arguments     ; An alist of non-subcommand arguments, by command
  (stdin  (generate-new-buffer " *doom-cli stdin*")  :type buffer)  ; buffer containing anything piped into this session
  (stdout (generate-new-buffer " *doom-cli stdout*") :type buffer)  ; buffer containing user-visible output
  (stderr (generate-new-buffer " *doom-cli stderr*") :type buffer)  ; buffer containing all output, including debug output
  ;; An alist of persistent and arbitrary elisp state
  (state  nil :type alist))

(defun doom-cli-context-execute (context)
  "Execute a given CONTEXT.

Use `doom-cli-context-parse' or `doom-cli-context-restore' to produce a valid,
executable context."
  (let* ((command (doom-cli-context-command context))
         (cli (doom-cli-get command t))
         (prefix (doom-cli-context-prefix context)))
    (doom-log "doom-cli-context-execute: %s"
              (mapconcat #'doom-cli-command-string
                         (delq nil (list (car (doom-cli-context-path context)) command))
                         " -> "))
    (cond ((null (or command (doom-cli-get (list prefix) t)))
           (signal 'doom-cli-invalid-prefix-error (list prefix)))

          ((doom-cli-context-meta-p context)
           (pcase (doom-cli-context-meta-p context)
             ("--version"
              (doom-cli-call `(:version ,@(cdr command)) context)
              t)
             ((or "-?" "--help")
              (doom-cli-call `(:help ,@(cdr command)) context)
              t)
             (_ (error "In meta mode with no destination!"))))

          ((not (and cli (doom-cli-fn (doom-cli-get cli))))
           (signal 'doom-cli-command-not-found-error
                   (append command (alist-get t (doom-cli-context-arguments context)))))

          ((let ((seen '(t))
                 runners)
             (dolist (cli (doom-cli-find command (doom-cli-type cli)))
               (push (cons (doom-cli-get cli)
                           (doom-cli--bindings cli context seen))
                     runners))
             (pcase-dolist (`(,cli . ,bindings) (nreverse runners))
               (doom-cli-execute cli bindings))
             context)))))

(defun doom-cli-context-restore (file context)
  "Restore the last restarted context from FILE into CONTEXT."
  (when (and (stringp file)
             (file-exists-p file))
    (when-let (old-context (with-temp-buffer
                             (insert-file-contents file)
                             (read (current-buffer))))
      (unless (doom-cli-context-p old-context)
        (error "An invalid context was restored from file: %s" file))
      (unless (equal (doom-cli-context-prefix context)
                     (doom-cli-context-prefix old-context))
        (error "Restored context belongs to another script: %s"
               (doom-cli-context-prefix old-context)))
      (pcase-dolist (`(,slot ,_ . ,plist)
                     (cdr (cl-struct-slot-info 'doom-cli-context)))
        (unless (plist-get plist :skip)
          (let* ((idx (cl-struct-slot-offset 'doom-cli-context slot))
                 (old-value (aref old-context idx)))
            (aset context idx
                  (pcase (plist-get plist :type)
                    (`alist
                     (dolist (entry old-value (aref context idx))
                       (setf (alist-get (car entry) (aref context idx)) (cdr entry))))
                    (`buffer
                     (with-current-buffer (aref context idx)
                       (insert old-value)
                       (current-buffer)))
                    (_ old-value))))))
      (run-hook-with-args 'doom-cli-create-context-functions context)
      (delete-file file)
      (doom-log "Restored context: %s" (doom-cli-context-pid context))
      context)))

(defun doom-cli-context-parse (args context)
  "Parse ARGS and update CONTEXT to reflect it."
  (let* ((case-fold-search t)
         (args (delq nil (copy-sequence args)))
         (arguments)
         rest?
         arg)
    (while args
      (setq arg (pop args))
      (save-match-data
        (cond
         ((equal arg "--")
          (doom-log "Found arg separator" arg)
          (setq arguments (cdr args)
                args nil))

         ((and (stringp arg)
               (string-match "^\\(-\\([^-]\\{2,\\}\\)\\)" arg))
          (let ((chars (split-string (match-string 2 arg) "" t)))
            (dolist (ch (nreverse chars))
              (push (concat "-" ch) args))))

         ((and (stringp arg)
               (or (string-match "^\\(--\\w[a-z0-9-_]+\\)\\(?:=\\(.*\\)\\)?$" arg)
                   (string-match "^\\(-[^-]\\)$" arg)))
          (doom-log "Found switch: %s" arg)
          (catch :skip
            (let* ((fullflag (match-string 1 arg))
                   (normflag (if (string-prefix-p "--no-" fullflag)
                                 (concat "--" (substring fullflag 5))
                               fullflag))
                   (option (or (doom-cli-context-find-option context normflag)
                               (when (member fullflag '("-?" "--help" "--version"))
                                 (doom-log "Found help switch: %s" arg)
                                 (setf (doom-cli-context-meta-p context) fullflag)
                                 (throw :skip t))
                               (when rest?
                                 (push arg arguments)
                                 (throw :skip t))
                               (signal 'doom-cli-unrecognized-option-error
                                       (list fullflag))))
                   (explicit-arg (match-string 2 arg))
                   (argsleft (+ (length args) (if explicit-arg 1 0)))
                   (arity (length (doom-cli-option-arguments option)))
                   (key (if (doom-cli-option-multiple-p option)
                            (car (doom-cli-option-switches option))
                          normflag)))
              (doom-cli-context-put
               context key
               (let ((value (seq-take args arity)))
                 (when explicit-arg
                   (push explicit-arg value))
                 (when (/= (length value) arity)
                   (signal 'doom-cli-wrong-number-of-arguments-error
                           (list (doom-cli--command context)
                                 fullflag value arity arity)))
                 (setq args  (seq-drop args arity)
                       value (apply #'doom-cli-option-validate option value))
                 (cond ((doom-cli-option-flag-p option)
                        (if (string-prefix-p "--no-" fullflag) :no :yes))
                       ((doom-cli-option-multiple-p option)
                        (append (doom-cli-context-get context key)
                                (if (doom-cli-option-arguments option)
                                    (cl-loop for v in value
                                             collect (cons fullflag v))
                                  (list fullflag))))
                       ((= arity 1) (car value))
                       ((> arity 1) value)
                       (fullflag)))))))

         ((when-let*
              (((null arguments))
               ((not rest?))
               (command (append (doom-cli-context-command context) (list arg)))
               (cli  (doom-cli-get command t))
               (rcli (doom-cli-get command))
               (key  (doom-cli-key rcli)))
            (doom-log "doom-cli-context-execute: found %s" command)
            ;; Show warnings depending on CLI plists
            (when (doom-cli-alias cli)
              (dolist (pcli (doom-cli-path cli))
                (doom-log "doom-cli-context-execute: path=%s" (doom-cli-key pcli))
                (push (doom-cli-key pcli) (doom-cli-context-path context))))
            ;; Collect &rest for this command
            (setf (doom-cli-context-command context) key
                  (map-elt (doom-cli-context-arguments context)
                           (doom-cli-command rcli))
                  (copy-sequence args))
            ;; Initialize options associated with this command to a nil value;
            ;; this simplifies existence validation later.
            (dolist (cli (doom-cli-find key))
              (dolist (option (doom-cli-options cli))
                (dolist (switch (doom-cli-option-switches option))
                  (unless (assoc switch (doom-cli-context-options context))
                    (setf (map-elt (doom-cli-context-options context) switch)
                          nil)))))
            ;; If this command uses &rest, stop processing commands from this
            ;; point on and pass the rest (of the unprocessed arguments) to it.
            (when (and (doom-cli-fn rcli)
                       (alist-get '&rest (doom-cli-arguments rcli)))
              (setq rest? t))
            t))

         ((push arg arguments)
          (doom-log "Found argument: %s" arg)))))

    (setf (alist-get t (doom-cli-context-arguments context))
          (append (alist-get t (doom-cli-context-arguments context))
                  (nreverse arguments)))
    (run-hook-with-args 'doom-cli-create-context-functions context)
    context))

(defun doom-cli-context-get (context key &optional null-value)
  "Fetch KEY from CONTEXT's options or state.

Context objects are essentially persistent storage, and may contain arbitrary
state tied to switches (\"--foo\" or \"-x\") or arbitrary symbols (state).

If KEY is a string, fetch KEY from context's OPTIONS (by switch).
If KEY is a symbol, fetch KEY from context's STATE.
Return NULL-VALUE if KEY does not exist."
  (if-let (value
           (if (stringp key)
               (assoc key (doom-cli-context-options context))
             (assq key (doom-cli-context-state context))))
      (cdr value)
    null-value))

(defun doom-cli-context-put (context key val)
  "Set KEY in CONTEXT's options or state to VAL.

Context objects contain persistent storage, and may contain arbitrary state tied
to switches (\"--foo\" or \"-x\") or arbitrary symbols (state). Use this to
register data into CONTEXT.

If KEY is a string, set the value of a switch named KEY to VAL.
If KEY is a symbol, set the value of the context's STATE to VAL."
  (setf (alist-get
         key (if (stringp key)
                 (doom-cli-context-options context)
               (doom-cli-context-state context))
         nil nil #'equal)
        val))

(defun doom-cli-context-find-option (context switch)
  "Return a `doom-cli-option' belonging to SWITCH in CONTEXT, if available.

Returns nil if SWITCH isn't a valid option in CONTEXT or none of the associated
`doom-cli's have a `doom-cli-option' associated with SWITCH."
  (when (assoc switch (doom-cli-context-options context))
    (cl-loop with command = (doom-cli-context-command context)
             for cli in (doom-cli-find command)
             if (seq-find (lambda (opt)
                            (let ((switches (doom-cli-option-switches opt)))
                              (or (member switch switches)
                                  (and (doom-cli-option-flag-p opt)
                                       (string-prefix-p "--no-" switch)))))
                          (doom-cli-options cli))
             return it)))

(defun doom-cli-context-width (context)
  "Return the width (in character units) of CONTEXT's original terminal."
  (or (car (doom-cli-context-geometry context))
      80))

(defun doom-cli-context-height (context)
  "Return the height (in character units) of CONTEXT's original terminal."
  (or (cdr (doom-cli-context-geometry context))
      40))

(defun doom-cli-context-pipe-p (context type &optional global?)
  "Return non-nil if TYPE is an active pipe in the local CONTEXT.

TYPE can be one of `:in' (receiving input on stdin) or `:out' (output is piped
to another process), or any of `local-in', `local-out', `global-in', or
`global-out'.

If GLOBAL? is non-nil, if TYPE is `:in' or `:out', the global context (the pipes
active in the super-session, rather than the local Emacs instance) will be
considered as well."
  (let ((pipes (doom-cli-context-pipes context)))
    (and (if global?
             (assq type pipes)
           (member (cons type 'local) pipes))
         t)))

(defun doom-cli-context-sid (context &optional nodate?)
  "Return a unique session identifier for CONTEXT."
  (if nodate?
      (doom-cli-context-pid context)
    (format (format-time-string
             "%y%m%d%H%M%S.%%s" (doom-cli-context-init-time context))
            (doom-cli-context-pid context))))


;;
;;; Output management

(defun doom-cli-debugger (type data &optional context)
  "Print a more presentable backtrace to terminal and write it to file."
  ;; HACK Works around a heuristic in eval.c for detecting errors in the
  ;;   debugger, which executes this handler again on subsequent calls. Taken
  ;;   from `ert--run-test-debugger'.
  (cl-incf num-nonmacro-input-events)
  (let* ((inhibit-read-only nil)
         (inhibit-message nil)
         (inhibit-redisplay nil)
         (inhibit-trace t)
         (executing-kbd-macro nil)
         (load-read-function #'read)
         (backtrace (doom-backtrace))
         (context (or context (doom-cli-context-create)))
         (straight-error
          (and (bound-and-true-p straight-process-buffer)
               (member straight-process-buffer data)
               (with-current-buffer (straight--process-buffer)
                 (split-string (buffer-string) "\n" t))))
         (error-file (doom-cli--output-file 'error context)))
    (cond
     (straight-error
      (print! (error "The package manager threw an error"))
      (print! (error "Last %d lines of straight's error log:")
              doom-cli-straight-error-lines)
      (print-group!
       (print!
        "%s" (string-join
              (seq-subseq straight-error
                          (max 0 (- (length straight-error)
                                    doom-cli-straight-error-lines))
                          (length straight-error))
              "\n")))
      (print! (warn "Wrote extended straight log to %s")
              (path (let ((coding-system-for-write 'utf-8-auto))
                      (with-temp-file error-file
                        (insert-buffer-substring (straight--process-buffer)))
                      (set-file-modes error-file #o600)
                      error-file))))
     ((eq type 'error)
      (let* ((generic? (eq (car data) 'error))
             (doom-cli-backtrace-depth doom-cli-backtrace-depth)
             (print-escape-newlines t))
        (if (doom-cli-context-p context)
            (print! (error "There was an unexpected runtime error"))
          (print! (bold (error "There was a fatal initialization error"))))
        (print-group!
         (print! "%s %s" (bold "Message:")
                 (if generic?
                     (error-message-string data)
                   (get (car data) 'error-message)))
         (unless generic?
           (print! "%s %s" (bold "Details:")
                   (let* ((print-level 3)
                          (print-circle t)
                          (print-escape-newlines t))
                     (prin1-to-string (cdr data)))))
         (when backtrace
           (print! (bold "Backtrace:"))
           (print-group!
            (dolist (frame (seq-take backtrace doom-cli-backtrace-depth))
              (print! "%s" (truncate (prin1-to-string
                                      (cons (backtrace-frame-fun  frame)
                                            (backtrace-frame-args frame)))
                                     (- (doom-cli-context-width context)
                                        doom-print-indent
                                        1)
                                     "..."))))
           (when-let (backtrace-file (doom-backtrace-write-to-file backtrace error-file))
             (print! (warn "Wrote extended backtrace to %s")
                     (path backtrace-file))))))))
    (exit! 255)))

(defun doom-cli--output-file (type context)
  "Return a log file path for TYPE and CONTEXT.

See `doom-cli-log-file-format' for details."
  (format doom-cli-log-file-format
          (doom-cli-context-prefix context)
          (doom-cli-context-sid context)
          type))

(defun doom-cli--output (out &optional context)
  "A `standard-output' function which mirrors output to log buffers."
  (let ((str (char-to-string out)))
    (dolist (buffer (list (doom-cli-context-stdout context)
                          (doom-cli-context-stderr context)))
      (when (bufferp buffer)
        (princ str buffer)))
    (send-string-to-terminal str)))

(defun doom-cli--output-write-logs-h (context)
  "Write all log buffers to their appropriate files."
  ;; Delete the last `doom-cli-log-retain' logs
  (mapc #'delete-file
        (let ((prefix (doom-cli-context-prefix context)))
          (append (butlast (doom-glob (format doom-cli-log-file-format prefix "*" "log"))
                           doom-cli-log-retain)
                  (butlast (doom-glob (format doom-cli-log-file-format prefix "*" "error"))
                           doom-cli-log-retain))))
  ;; Then write the log file, if necessary
  (let* ((buffer (doom-cli-context-stderr context))
         (file (doom-cli--output-file "log" context)))
    (when (> (buffer-size buffer) 0)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert-buffer-substring buffer)
        (ansi-color-filter-region (point-min) (point-max)))
      (set-file-modes file #o600))))

(defun doom-cli--output-benchmark-h (context)
  "Write this session's benchmark to stdout or stderr, depending.

Will also output it to stdout if requested (CLI sets :benchmark to t) or the
command takes >5s to run. If :benchmark is explicitly set to nil (or
`doom-cli-benchmark-threshold' is nil), under no condition should a benchmark be
shown."
  (when-let* ((init-time (doom-cli-context-init-time context))
              (cli (doom-cli-get context))
              (duration (float-time (time-subtract (current-time) init-time)))
              (hours    (/ (truncate duration) 60 60))
              (minutes  (- (/ (truncate duration) 60) (* hours 60)))
              (seconds  (- duration (* hours 60 60) (* minutes 60)))
              (standard-output
               (if (and (/= doom-cli--exit-code 0)
                        (or init-file-debug
                            (eq (doom-cli-prop cli :benchmark) t)
                            (and (eq (doom-cli-prop cli :benchmark :null) :null)
                                 (not (doom-cli-context-pipe-p context 'out t))
                                 (> duration (or doom-cli-benchmark-threshold
                                                 most-positive-fixnum)))))
                   (doom-rpartial #'doom-cli--output context)
                 (doom-cli-context-stderr context))))
    (print! (success "Finished in %s")
            (join (list (unless (zerop hours)   (format "%dh" hours))
                        (unless (zerop minutes) (format "%dm" minutes))
                        (format (if (> duration 60) "%ds" "%.5fs")
                                seconds))))
    (doom-log "GC count: %d (%.6fs)" gcs-done gc-elapsed)))

(defun doom-cli--redirect-output-a (context message &rest args)
  ":override advice for `message' to mirror output to log buffers"
  (when message
    (let ((output (apply #'doom-print--format message args)))
      ;; One for the terminal, if the log level is high enough.
      (doom-print output :format nil :level doom-print-message-level)
      ;; And one for the logs...
      (when (doom-cli-context-p context)
        (doom-print output :format nil :stream (doom-cli-context-stderr context) :level t)))
    message))


;;
;;; Session management

(defun doom-cli-call (args context &optional error)
  "Process ARGS (list of string shell arguments) with CONTEXT as the basis.

If ERROR is provided, store the error in CONTEXT, in case a later CLI wants to
read/use it (e.g. like a :help CLI)."
  (let ((oldcommand (doom-cli-context-command context)))
    (if oldcommand
        (doom-log "doom-cli-call: %s -> %s" oldcommand args)
      (doom-log "doom-cli-call: %s" oldcommand args))
    (when error
      (setf (doom-cli-context-error context) error))
    (setf (doom-cli-context-command context) nil
          (doom-cli-context-arguments context) nil
          (doom-cli-context-meta-p context) nil)
    (doom-cli-context-execute
     (doom-cli-context-parse args (or context doom-cli--context)))))

(defun doom-cli--restart (args context)
  "Restart the current CLI session.

If CONTEXT is non-nil, this is written to file and restored in the next Doom
session.

This is done by writing a temporary shell script, which is executed after this
session ends (see the shebang lines of this file). It's done this way because
Emacs' batch library lacks an implementation of the exec system call."
  (unless (doom-cli-context-p context)
    (error "Attempted `doom-cli--restart' without active context"))
  (when (= (doom-cli-context-step context) -1)
    (error "__DOOMSTEP envvar missing; extended `exit!' functionality will not work"))
  (let* ((pid  (doom-cli-context-pid context))
         (step (doom-cli-context-step context))
         (context-file (format (doom-path temporary-file-directory "doom.%s.%s.context") pid step))
         (script-file  (format (doom-path temporary-file-directory "doom.%s.%s.sh") pid step))
         (command (if (listp args) (combine-and-quote-strings (remq nil args)) args))
         (coding-system-for-write 'utf-8-auto)
         (coding-system-for-read  'utf-8-auto))
    (cl-incf (doom-cli-context-step context))
    (make-directory (file-name-directory context-file) t)
    (with-temp-file context-file
      ;; DEPRECATED Use `print-unreadable-function' when 28 support is dropped
      (let ((newcontext (doom-cli-context-copy context))
            (print-level nil)
            (print-length nil)
            (print-circle nil)
            (print-escape-newlines t))
        (letf! (defmacro convert-buffer (fn)
                 `(setf (,fn newcontext) (with-current-buffer (,fn context)
                                           (buffer-string))))
          (convert-buffer doom-cli-context-stdin)
          (convert-buffer doom-cli-context-stdout)
          (convert-buffer doom-cli-context-stderr))
        (prin1 newcontext (current-buffer))))
    (set-file-modes context-file #o400)
    (setenv "__DOOMCONTEXT" context-file)
    (make-directory (file-name-directory script-file) t)
    (with-temp-file script-file
      (setq-local coding-system-for-write 'utf-8-auto)
      (insert "#!/usr/bin/env sh\n"
              "trap _doomcleanup EXIT\n"
              "_doomcleanup() {\n"
              "  rm -" (if init-file-debug "v" "") "f "
              (combine-and-quote-strings (delq nil (list script-file context-file)))
              "\n}\n"
              "_doomrun() {\n  " command "\n}\n"
              (save-match-data
                (cl-loop with initial-env = (get 'process-environment 'initial-value)
                         for env in (seq-difference process-environment initial-env)
                         if (string-match "^\\([a-zA-Z0-9_]+\\|__DOOM[^=]+\\)=\\(.+\\)$" env)
                         concat (format "%s=%s \\\n"
                                        (match-string 1 env)
                                        (shell-quote-argument (match-string 2 env)))))
              (format "PATH=\"%s%s$PATH\" \\\n"
                      (doom-path doom-emacs-dir "bin")
                      path-separator)
              "_doomrun \"$@\"\n"))
    (set-file-modes script-file #o600)
    ;; Error code 254 is special: it indicates to the caller that the
    ;; post-script should be executed after this session ends. It's up to
    ;; `doom-cli-run's caller to enforce this (see bin/doom's shebang for a
    ;; comprehensive example).
    (doom-cli--exit 254 context)))

(defun doom-cli--exit (args context)
  "Accepts one of the following:

  (CONTEXT [ARGS...])
    TODO
  (STRING [ARGS...])
    TODO
  (:restart [ARGS...])
    TODO
  (:pager [FILE...])
    TODO
  (:pager? [FILE...])
    TODO
  (INT)
    TODO"
  (let ((command (or (car-safe args) args))
        (args    (if (car-safe args) (cdr-safe args))))
    (pcase command
      ;; If an integer, treat it as an exit code.
      ((pred (integerp))
       (setq doom-cli--exit-code command)
       (kill-emacs command))

      ;; Otherwise, run a command verbatim.
      ((pred (stringp))
       (doom-cli--restart (format "%s %s" command (combine-and-quote-strings args))
                          context))

      ;; Same with buffers.
      ((pred (bufferp))
       (doom-cli--restart (with-current-buffer command (buffer-string))
                          context))

      ;; If a context is given, restart the current session with the new context.
      ((pred (doom-cli-context-p))
       (doom-cli--exit-restart args command))

      ;; Run a custom action, defined in `doom-cli-exit-commands'.
      ((pred (keywordp))
       (if-let (fn (alist-get command doom-cli-exit-commands))
           (funcall fn args context)
         (error "Invalid exit command: %s" command)))

      ;; Any other value is invalid.
      (_ (error "Invalid exit code or command: %s" command)))))

(defun doom-cli--exit-restart (args context)
  "Restart the session, verbatim (persisting CONTEXT)."
  (doom-cli--exit (cons "$@" args) context))

(defun doom-cli--exit-pager (args context)
  "Invoke pager on output unconditionally.

ARGS are options passed to less. If DOOMPAGER is set, ARGS are ignored."
  (let ((pager (or doom-cli-pager (getenv "DOOMPAGER"))))
    (cond ((null (or pager (executable-find "less")))
           (user-error "No pager set or available")
           (doom-cli--exit 1 context))

          ((or (doom-cli-context-pipe-p context :out t)
               (equal pager ""))
           (doom-cli--exit 0 context))

          ((let ((tmpfile (doom-cli--output-file 'output context))
                 (coding-system-for-write 'utf-8-auto))
             (make-directory (file-name-directory tmpfile) t)
             (with-temp-file tmpfile
               (insert-buffer-substring (doom-cli-context-stdout context)))
             (set-file-modes tmpfile #o600)
             (doom-cli--restart
              (format "%s <%s; rm -f%s %s"
                      (or pager
                          (format "less %s"
                                  (combine-and-quote-strings
                                   (append (if doom-print-backend '("-r")) ; process ANSI codes
                                           (or (delq nil args) '("+g"))))))
                      (shell-quote-argument tmpfile)
                      (if init-file-debug "v" "")
                      (shell-quote-argument tmpfile))
              context))))))

(defun doom-cli--exit-pager-maybe (args context)
  "Invoke pager if stdout is longer than TTY height * `doom-cli-pager-ratio'.

ARGS are options passed to less. If DOOMPAGER is set, ARGS are ignored."
  (doom-cli--exit
   (let ((threshold (ceiling (* (doom-cli-context-height context)
                                doom-cli-pager-ratio))))
     (if (>= (let ((stdout (doom-cli-context-stdout context)))
               (if (fboundp 'buffer-line-statistics)
                   (car (buffer-line-statistics stdout))
                 (with-current-buffer stdout
                   (count-lines (point-min) (point-max)))))
             threshold)
         (cons :pager args)
       0))
   context))

;; (defun doom-cli--exit-editor (args context))  ; TODO Launch $EDITOR

;; (defun doom-cli--exit-emacs (args context))   ; TODO Launch Emacs subsession


;;
;;; Misc

(defun doom-cli-load (cli)
  "If CLI is autoloaded, load it, otherwise return it unchanged."
  (or (when-let* ((path (doom-cli-autoload cli))
                  (path (locate-file-internal path doom-cli-load-path load-suffixes)))
        (doom-log "Autoloading %s" path)
        (let ((doom-cli--plist (doom-cli-plist cli)))
          (load! path))
        (let* ((key (doom-cli-key cli))
               (cli (gethash key doom-cli--table)))
          (when (doom-cli-autoload cli)
            (signal 'doom-cli-autoload-error (list (doom-cli-command cli) path)))
          cli))
      cli))

(defun doom-cli-load-all ()
  "Immediately load all autoloaded CLIs."
  (dolist (key (hash-table-keys doom-cli--table))
    (doom-cli-load (gethash key doom-cli--table))))


;;
;;; DSL

(defmacro defcli! (commandspec arglist &rest body)
  "Defines a CLI command.

COMMANDSPEC is the specification for the command that will trigger this CLI. It
can either be a symbol or list of symbols (or nested symbols). Nested lists are
treated as a list of aliases for the command. For example:

  (defcli! doom () ...)              ; invoked on 'doom'
  (defcli! (doom foo) () ...)        ; invoked on 'doom foo'
  (defcli! (doom (foo bar)) () ...)  ; invoked on 'doom foo' or 'doom bar'

COMMANDSPEC may be prefixed with any of these special keywords:

  :root ...
    This command will ignore any :prefix set by a parent `defgroup!'.
  :before ...
    This command will run before the specified command(s).
  :after ...
    This command will run after the specified command(s).
  :version
    A special handler, executed when 'X --version' is called. Define your own,
    if you don't want it spewing Doom's version information.
  :help COMMAND...
    A special handler, executed when help documentation is requested for a
    command. E.g. 'doom help foo' or 'doom foo --help' will call (:help foo).
    You can define your own global :help handler, or one for a specific command.
  :dump COMMAND...
    A special handler, executed when the __DOOMDUMP environment variable is set.
    You can define one for a specific COMMAND, or omit it to redefine the
    catch-all :dump handler.

    The default implementation (living in core/core-cli.el) will either:

    a) Dump to stdout a list of `doom-cli' structs for the commands and pseudo
       commands that would've been executed had __DOOMDUMP not been set.
    b) Or, given only \"-\" as an argument, dump all of `doom-cli--table' to
       stdout. This table contains all known `doom-cli's (after loading
       autoloaded ones).

To interpolate values into COMMANDSPEC (e.g. to dynamically generate commands),
use the comma operator:

  (let ((somevar 'bfg))
    (defcli! (doom ,somevar) ...))

DOCSTRING is a string description; its first line should be a short summary
(under 60 characters) of what the command does. It will be used in the cramped
command listings served by help commands. The rest of DOCSTRING lines should be
no longer than 80 columns, and should go into greater detail. This documentation
may use `quoting' to appropriately highlight ARGUMENTS, --options, or $ENVVARS.

DOCSTRING may also contain sections denoted by a capitalized header ending with
a colon and newline, and its contents indented by 2 spaces. These will be
appended to the end of the help documentation for that command. These three
sections are special:

  ARGUMENTS:
    Use this to specify longer-form documentation for arguments. They are
    prepended to the documentation for commands. If pseudo CLIs specify their
    own ARGUMENTS sections, they are joined with that of the root command's CLI
    as well. E.g. ':before doom sync's ARGUMENTS will be prepended to 'doom
    sync's.
  OPTIONS:
    Use this to specify longer-form documentation for options. They are appended
    to the auto-generated section of the same name. Only the option needs to be
    specified for its lookup behavior to work. See bin/doom's `doom' command as
    an example.
  EXAMPLES:
    To list example uses of the containing script. These are appended to
    SYNOPSIS in generated manpages, but treated as a normal section otherwise
    (i.e. appended to 'doom help's output).

DOCSTRING may use any of these format specifications:

  %p  The running script's prefix. E.g. for 'doom ci deploy-hooks' the
      prefix is 'doom'.
  %c  The parent command minus the prefix. E.g. for 'doom ci deploy-hooks',
      the command is 'ci deploy-hooks'.

ARGLIST is a specification for options and arguments that is accepted by this
command. Arguments are represented by either a symbol or a cons cell where
(SYMBOL . DOCUMENTATION), and option specifications are lists in the following
formats:

  ([TYPE] VAR (FLAGSPEC... [ARGSPEC...]) [DESCRIPTION])

  TYPE
    Optional. One of &flag or &multi (which correspond to &flags and &multiple,
    respectively, and are used for specifying a type inline, if desired).
  VAR
    Is the symbol to bind that option's value to.
  FLAGSPEC
    A list of switches or sub-lists thereof. Each switch is a string, e.g.
    \"--foo\" \"-b\" \"--baz\".

    Nested lists will be treated as logical groups of switches in documentation.
    E.g. for

    With (\"--foo\" \"--bar\" [ARGSPEC...]) you get:

      --foo, --bar
        [Documentation]

    With ((\"--foo\") (\"--bar\") [ARGSPEC...]) you get:

      --foo
      --bar
        [Documentation]

    Use this to logically group options that have many, but semantically
    distinct switches.
  ARGSPEC
    A list of arguments or sub-lists thereof. Each argument is either a string
    or symbol.

    If a string, they are used verbatim as the argument's documentation. Use
    this to document more complex specifications, like \"[user@]host[:port]\".
    Use reference `quotes' to highlight arguments appropriately.

    If a symbol, this is equivalent to (upcase (format \"`%s'\" SYMBOL)), but
    its arguments will also be implicitly validated against
    `doom-cli-option-arg-types'.

    A nested list indicates that an argument accepts multiple types, and are
    implicitly joined into \"`ARG1'|`ARG2'|...\".

    WARNING: If this option is a &flag, the option must not accept arguments.
    Instead, use ARGSPEC to specify a single, default value (one of `:yes' or
    `:no').
  DESCRIPTION
    A one-line description of the option. Use reference `quotes' to
    appropriately highlight arguments, options, and envvars. A syntax exists for
    adding long-form option documentation from the CLI's docstring. See
    DOCSTRING above.

ARGLIST may be segmented with the following auxiliary keywords:

  &args ARG
    The rest of the literal arguments are stored in ARG.
  &cli ARG
    The called `doom-cli' struct is bound to ARG.
  &context ARG
    The active `doom-cli-context' struct is bound to ARG.
  &flags OPTION...
    An option '--foo' declared after &flags will implicitly include a
    '--no-foo', and will appear as \"--[no-]foo\" in 'doom help' docs.
  &multiple OPTION...
    Options specified after &multiple may be passed to the command multiple
    times. Its symbol will be bound to a list of cons cells containing (FLAG .
    VALUE).
  &optional ARG...
    Indicates that the (literal) arguments after it are optional.
  &input ARG
    ARG will be bound to the input piped in from stdin, as a string, or nil if
    unavailable. If you want access to the original buffer, use
    (doom-cli-context-stdin context) instead.
  &rest ARG
    All switches and arguments, unprocessed, after this command. If given, any
    unrecognized switches will not throw an error. This will also prevent
    subcommands beneath this command from being recognized. Use with care!

  Any non-option arguments before &optional, &rest, or &args are required.

BODY is a list of arbitrary elisp forms that will be executed when this command
is called. BODY may begin with a plist to set metadata about it. The recognized
properties:

  :alias (CMD...)
    Designates this command is an alias to CMD, which is a command specification
    identical to COMMANDSPEC.
  :benchmark BOOL
    If non-nil, display a benchmark after the command finishes.
  :disable BOOL
    If non-nil, the command will not be defined.
  :docs STRING
    An alternative to DOCSTRING for defining documentation for this command.
  :group (STR...)
    A breadcrumb of group names to file this command under. They will be
    organized by category in the CLI documentation (available through SCRIPT
    {--help,-?,help}).
  :hide BOOL
    If non-nil, don't display this command in the help menu or in {ba,z}sh
    completion (though it will still be callable).
  :partial BOOL
    If non-nil, this command is treated as partial, an intermediary command
    intended as a stepping stone toward a non-partial command. E.g. were you to
    define (doom foo bar), two \"partial\" commands are implicitly created:
    \"doom\" and \"doom foo\". When called directly, partials will list its
    subcommands and complain that a subcommand is rqeuired, rather than display
    an 'unknown command' error.
  :prefix (STR...)
    A command path to prepend to the command name. This is more useful as part
    of `defgroup!'s inheritance.

The BODY of commands with a non-nil :alias, :disable, or :partial will be
ignored.

\(fn COMMANDSPEC ARGLIST [DOCSTRING] &rest BODY...)"
  (declare (indent 2) (doc-string 3))
  (let ((docstring (if (stringp (car body)) (pop body)))
        (plist (let (plist)
                 (while (keywordp (car body))
                   (push (pop body) plist)
                   (push (pop body) plist))
                 (nreverse plist)))
        options arguments bindings)
    (let ((type '&required))
      (dolist (arg arglist)
        (cond ((listp arg)
               (let* ((inline-type (cdr (assq (car arg) doom-cli-option-types)))
                      (type (or inline-type type))
                      (args (if inline-type (cdr arg) arg)))
                 (push (apply (or (alist-get type doom-cli-option-generators)
                                  (signal 'doom-cli-definition-error
                                          (cons "Invalid option type" type)))
                              args)
                       options)
                 (push (car args) bindings)))
              ((memq arg doom-cli-argument-types)
               (setq type arg))
              ((string-prefix-p "&" (symbol-name arg))
               (signal 'doom-cli-definition-error (cons "Invalid argument specifier" arg)))
              ((push arg bindings)
               (push arg (alist-get type arguments))))))
    (dolist (arg arguments)
      (setcdr arg (nreverse (cdr arg))))
    `(let (;; Define function early to prevent overcapturing
           (fn ,(let ((clisym   (make-symbol "cli"))
                      (alistsym (make-symbol "alist")))
                  `(lambda (,clisym ,alistsym)
                     (let ,(cl-loop for arg in (nreverse bindings)
                                    unless (string-prefix-p "_" (symbol-name arg))
                                    collect `(,arg (cdr (assq ',arg ,alistsym))))
                       ,@body)))))
       ;; `cl-destructuring-bind's will validate keywords, so I don't have to
       (cl-destructuring-bind
           (&whole plist &key
                   alias autoload _benchmark docs disable hide _group partial
                   _prefix)
           (append (list ,@plist) doom-cli--plist)
         (unless disable
           (let* ((command  (doom-cli-command-normalize (backquote ,commandspec) plist))
                  (type     (if (keywordp (car command)) (pop command)))
                  (commands (doom-cli--command-expand command t))
                  (target   (pop commands)))
             (dolist (prop '(:autoload :alias :partial :hide))
               (cl-callf map-delete plist prop))
             (puthash (delq nil (cons type target))
                      (doom-cli-create
                       :command target
                       :type type
                       :docs (doom-cli--parse-docs (or ',docstring docs))
                       :arguments ',arguments
                       :options ',(nreverse options)
                       :autoload autoload
                       :alias (if alias (doom-cli-command-normalize alias plist))
                       :plist (append plist (list :hide (and (or hide type) t)))
                       :fn (unless (or partial autoload) fn))
                      doom-cli--table)
             (let ((docs (doom-cli--parse-docs docs)))
               (dolist (alias (cl-loop for c in commands
                                       while (= (length c) (length target))
                                       collect (pop commands)))
                 (puthash (delq nil (cons type alias))
                          (doom-cli-create
                           :command alias
                           :type type
                           :docs docs
                           :autoload autoload
                           :alias (unless autoload (delq nil (cons type target)))
                           :plist (append plist '(:hide t)))
                          doom-cli--table))
               (dolist (partial commands)
                 (let ((cli (gethash partial doom-cli--table)))
                   (when (or (null cli) (doom-cli-autoload cli))
                     (puthash (delq nil (cons type partial))
                              (doom-cli-create
                               :command partial
                               :type type
                               :docs docs
                               :plist (list :group (plist-get plist :group)))
                              doom-cli--table)))))
             target))))))

(defmacro defalias! (commandspec target &rest plist)
  "Define a CLI alias for TARGET at COMMANDSPEC.

See `defcli!' for information about COMMANDSPEC.
TARGET is not a command specification, and should be a command list."
  `(defcli! ,commandspec () :alias ',target ,@plist))

(defmacro defobsolete! (commandspec target when)
  "Define an obsolete CLI COMMANDSPEC that refers users to NEW-COMMAND.

See `defcli!' for information about COMMANDSPEC.
TARGET is simply a command list.
WHEN specifies what version this command was rendered obsolete."
  `(let ((ncommand (doom-cli-command-normalize (backquote ,target) doom-cli--plist)))
     (defcli! ,commandspec (&context context &cli cli &rest args)
       :docs (format "An obsolete alias for '%s'." (doom-cli-command-string ncommand))
       :hide t
       (print! (warn "'%s' was deprecated in %s")
               (doom-cli-command-string cli)
               ,when)
       (print! (warn "It will eventually be removed; use '%s' instead.")
               (doom-cli-command-string ncommand))
       (call! ',target args))))

(defmacro defstub! (commandspec &optional _argspec &rest body)
  "Define a stub CLI, which will throw an error if invoked.

Use this to define commands that will eventually be implemented, but haven't
yet. They won't be included in command listings (by help documentation)."
  `(defcli! ,commandspec (&rest _)
     ,(concat "THIS COMMAND IS A STUB AND HAS NOT BEEN IMPLEMENTED YET."
              (if (stringp (car body)) (concat "\n\n" (pop body))))
     :hide t
     (user-error "Command not implemented yet")))

(defmacro defautoload! (commandspec &optional path &rest plist)
  "Defer loading of PATHS until PREFIX is called."
  `(let* ((doom-cli--plist (append (list ,@plist) doom-cli--plist))
          (commandspec (doom-cli-command-normalize ',commandspec))
          (commands (doom-cli--command-expand commandspec))
          (path (or ,path
                    (when-let* ((cmd  (car commands))
                                (last (car (last cmd)))
                                (last (if (listp last) (car last) last)))
                      (format "%s" last))
                    (error "Failed to deduce autoload path for: %s" spec)))
          (cli (doom-cli-get (car commands) nil t)))
     (when (or (null cli)
               (doom-cli-autoload cli))
       (defcli! ,commandspec () :autoload path))))

(defmacro defgroup! (&rest body)
  "Declare common properties for any CLI commands defined in BODY."
  (when (stringp (car body))
    (push :group body))
  `(let ((doom-cli--plist (copy-sequence doom-cli--plist)))
     ,@(let (forms)
         (while (keywordp (car body))
           (let ((key (pop body))
                 (val (pop body)))
             (push `(cl-callf plist-put doom-cli--plist
                      ,key ,(if (eq key :prefix)
                                `(append (plist-get doom-cli--plist ,key)
                                         (doom-enlist ,val))
                              val))
                   forms)))
         (nreverse forms))
     ,@body))

(defun exit! (&rest args)
  "Exits the current CLI session.

With ARGS, you may specify a shell command or action (see
`doom-cli-exit-commands') to execute after this Emacs process has ended. For
example:

  (exit! \"$@\") or (exit! :restart)
    This reruns the current command with the same arguments.
  (exit! \"$@ -h -c\")
    This reruns the current command with two new switches.
  (exit! \"emacs -nw FILE\")
    Opens Emacs on FILE
  (exit! \"emacs\" \"-nw\" \"FILE\")
    Opens Emacs on FILE, but each argument is escaped (and nils are ignored).
  (exit! t) or (exit! nil)
    A safe way to simply abort back to the shell with exit code 0
  (exit! 42)
    Abort to shell with an explicit exit code.
  (exit! context)
    Restarts the current session, but with context (a `doom-cli-context' struct).
  (exit! :pager [FILES...])
    Invoke $DOOMPAGER (or less) on the output of this session. If ARGS are given, launch the pager on those
  (exit! :pager? [FILES...])
    Same as :pager, but does so only if output is longer than the terminal is
    tall.

See `doom-cli--restart' for implementation details."
  (doom-cli--exit (flatten-list args) doom-cli--context))

(defun call! (&rest command)
  "A convenience wrapper around `doom-cli-call'.

Implicitly resolves COMMAND relative to the running CLI, and uses the active
context (so you don't have to pass a context)."
  (doom-cli-call (doom-cli-command-normalize
                  (flatten-list command)
                  `(:prefix
                    ,(doom-cli-context-prefix doom-cli--context)
                    ,@(doom-cli-context-command doom-cli--context)))
                 doom-cli--context))

(defun run! (prefix &rest args)
  "Parse and execute ARGS.

This is the entry point for any shell script that rely on Doom's CLI framework.
It should be called once, at top-level, and never again (use `doom-cli-call' for
nested calls instead).

PREFIX is the name (string) of the top-level shell script (i.e. $0). All
commands that belong to this shell session should use PREFIX as the first
segment in their command paths.

ARGS is a list of string arguments to execute.

See bin/doom's shebang for an example of what state needs to be initialized for
Doom's CLI framework. In a nutshell, Doom is expecting the following environment
variables to be set:

  __DOOMGEOM   The dimensions of the current terminal (W . H)
  __DOOMPIPE   Must contain 0 if script is being piped into, 1 if piping it out
  __DOOMGPIPE  Like __DOOMPIPE, but is the pipe state of the super process
  __DOOMPID    A unique ID for this session and its exit script processes
  __DOOMSTEP   How many layers deep this session has gotten

The script should also execute ${temporary-file-directory}/doom.sh if Emacs
exits with code 254. This script is auto-generated as needed, to simulate exec
syscalls. See `doom-cli--restart' for technical details.

Once done, this function kills Emacs gracefully and writes output to log files
(stdout to `doom-cli--output-file', stderr to `doom-cli-debug-file', and any
errors to `doom-cli-error-file')."
  (when doom-cli--context
    (error "Cannot nest `run!' calls"))
  (letf! ((args (flatten-list args))
          (context (doom-cli-context-create :prefix prefix :whole args))
          (doom-cli--context context)
          (write-logs-fn (doom-partial #'doom-cli--output-write-logs-h context))
          (show-benchmark-fn (doom-partial #'doom-cli--output-benchmark-h context))
          ;; Write more user-friendly backtraces
          (debugger (doom-rpartial #'doom-cli-debugger context))
          (debug-on-error t)
          ;; Clone output to stdout/stderr buffers for logging.
          (standard-output (doom-rpartial #'doom-cli--output context))
          (#'message (doom-partial #'doom-cli--redirect-output-a context)))
    (doom-log "Starting!")
    (add-hook 'kill-emacs-hook show-benchmark-fn 94)
    (add-hook 'kill-emacs-hook write-logs-fn 95)
    (when (doom-cli-context-pipe-p context :out t)
      (setq doom-print-backend nil))
    (when (doom-cli-context-pipe-p context :in)
      (with-current-buffer (doom-cli-context-stdin context)
        (while (if-let (in (ignore-errors (read-from-minibuffer "")))
                   (insert in "\n")
                 (ignore-errors (delete-char -1))))))
    (doom-log "doom-cli-run: %s" command-line-args)
    (doom-cli--exit
     (condition-case e
         (let* ((args (cons (if (getenv "__DOOMDUMP") :dump prefix) args))
                (context
                 (or (doom-cli-context-restore (getenv "__DOOMCONTEXT") context)
                     (doom-cli-context-parse args context))))
           (run-hook-with-args 'doom-cli-before-run-functions context)
           (let ((result (doom-cli-context-execute context)))
             (run-hook-with-args 'doom-cli-after-run-functions context result))
           0)
       (doom-cli-wrong-number-of-arguments-error
        (pcase-let ((`(,command ,flag ,args ,min ,max) (cdr e)))
          (print! (red "Error: %S expected %s argument%s, but got %d")
                  (or flag (doom-cli-command-string
                            (if (keywordp (car command))
                                command
                              (cdr command))))
                  (if (or (= min max)
                          (= max most-positive-fixnum))
                      min
                    (format "%d-%d" min max))
                  (if (or (= min 0) (> min 1)) "s" "")
                  (length args))
          (doom-cli-call `(:help "--synopsis" "--postamble" ,@(cdr (doom-cli--command context))) context e))
        5)
       (doom-cli-unrecognized-option-error
        (print! (red "Error: unknown option %s") (cadr e))
        (doom-cli-call `(:help "--synopsis" "--postamble" ,@(cdr (doom-cli--command context))) context e)
        5)
       (doom-cli-invalid-option-error
        (pcase-let ((`(,types ,option ,value ,errors) (cdr e)))
          (print! (red "Error: %s received invalid value %S")
                  (string-join (doom-cli-option-switches option) "/")
                  value)
          (print! (bold "\nValidation errors:"))
          (dolist (err errors) (print! (item "%s." (fill err)))))
        (doom-cli-call `(:help "--postamble" ,@(cdr (doom-cli--command context))) context e)
        5)
       (doom-cli-command-not-found-error
        (let* ((command (cdr e))
               (cli (doom-cli-get command)))
          (cond ((null cli)
                 (print! (red "Error: unrecognized command '%s'")
                         (doom-cli-command-string (or (cdr command) command)))
                 (doom-cli-call `(:help "--similar" "--postamble" ,@(cdr command)) context e))
                ((null (doom-cli-fn cli))
                 (print! (red "Error: a subcommand is required"))
                 (doom-cli-call `(:help "--subcommands" "--postamble" ,@(cdr command)) context e))))
        4)
       (doom-cli-invalid-prefix-error
        (let ((prefix (cadr e)))
          (print! (red "Error: `run!' called with invalid prefix %S") prefix)
          (if-let (suggested (cl-loop for cli being the hash-value of doom-cli--table
                                      unless (doom-cli-type cli)
                                      return (car (doom-cli-command cli))))
              (print! "Did you mean %S?" suggested)
            (print! "There are no commands defined under %S." prefix)))
        4)
       (user-error
        (print! (red "Error: %s") (cadr e))
        (print! "\nAborting...")
        3))
     context)))

(defalias 'sh! #'doom-call-process)

(defalias 'sh!! #'doom-exec-process)

;; TODO Make `git!' into a more sophisticated wrapper around git
(defalias 'git! (doom-partial #'straight--process-run "git"))

(provide 'core-cli-lib)
;;; core-cli-lib.el ends here
