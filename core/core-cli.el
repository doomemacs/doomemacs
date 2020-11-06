;;; core/core-cli.el --- -*- lexical-binding: t; no-byte-compile: t; -*-

(load! "autoload/process")
(load! "autoload/plist")
(load! "autoload/files")
(load! "autoload/output")
(require 'seq)

;; Create all our core directories to quell file errors.
(mapc (doom-rpartial #'make-directory 'parents)
      (list doom-local-dir
            doom-etc-dir
            doom-cache-dir))

;; Ensure straight and the bare minimum is ready to go
(require 'core-modules)
(require 'core-packages)
(doom-initialize-core-packages)

;; Don't generate superfluous files when writing temp buffers
(setq make-backup-files nil)

;; Stop user configuration from interfering with Doom
(setq enable-dir-local-variables nil)


;;
;;; Variables

(defvar doom-auto-accept (getenv "YES")
  "If non-nil, Doom will auto-accept any confirmation prompts during batch
commands like `doom-cli-packages-install', `doom-cli-packages-update' and
`doom-packages-autoremove'.")

(defvar doom-auto-discard (getenv "FORCE")
  "If non-nil, discard all local changes while updating.")

(defvar doom-cli-file "cli"
  "The basename of CLI config files for modules.

These are loaded when a Doom's CLI starts up. There users and modules can define
additional CLI commands, or reconfigure existing ones to better suit their
purpose.")

(defvar doom-cli-log-file (concat doom-local-dir "doom.log")
  "File to write the extended output to.")

(defvar doom-cli-log-error-file (concat doom-local-dir "doom.error.log")
  "File to write the last backtrace to.")

(defvar doom--cli-commands (make-hash-table :test 'equal))
(defvar doom--cli-groups (make-hash-table :test 'equal))
(defvar doom--cli-group nil)

(define-error 'doom-cli-error "There was an unexpected error" 'doom-error)
(define-error 'doom-cli-command-not-found-error "Could not find that command" 'doom-cli-error)
(define-error 'doom-cli-wrong-number-of-arguments-error "Wrong number of CLI arguments" 'doom-cli-error)
(define-error 'doom-cli-unrecognized-option-error "Not a recognized option" 'doom-cli-error)
(define-error 'doom-cli-deprecated-error "Command is deprecated" 'doom-cli-error)


;;
;;; CLI library

(cl-defstruct
    (doom-cli
     (:constructor nil)
     (:constructor
      make-doom-cli
      (name &key desc aliases optlist arglist plist fn
            &aux
            (optlist
             (cl-loop for (symbol options desc) in optlist
                      for ((_ . options) (_ . params))
                      = (seq-group-by #'stringp options)
                      collect
                      (make-doom-cli-option :symbol symbol
                                            :flags options
                                            :args params
                                            :desc desc))))))
  (name nil :read-only t)
  (desc "TODO")
  aliases
  optlist
  arglist
  plist
  (fn (lambda (_) (print! "But nobody came!"))))

(cl-defstruct doom-cli-option
  (symbol)
  (flags ())
  (args ())
  (desc "TODO"))

(defun doom--cli-get-option (cli flag)
  (cl-find-if (doom-partial #'member flag)
              (doom-cli-optlist cli)
              :key #'doom-cli-option-flags))

(defun doom--cli-process (cli args)
  (let* ((args (copy-sequence args))
         (arglist (copy-sequence (doom-cli-arglist cli)))
         (expected
          (or (cl-position-if (doom-rpartial #'memq cl--lambda-list-keywords)
                              arglist)
              (length arglist)))
         (got 0)
         restvar
         rest
         alist)
    (catch 'done
      (while args
        (let ((arg (pop args)))
          (cond ((eq (car arglist) '&rest)
                 (setq restvar (cadr arglist)
                       rest (cons arg args))
                 (throw 'done t))

                ((string-match "^\\(--\\([a-zA-Z0-9][a-zA-Z0-9-_]*\\)\\)\\(?:=\\(.+\\)\\)?$" arg)
                 (let* ((fullflag (match-string 1 arg))
                        (opt (doom--cli-get-option cli fullflag)))
                   (unless opt
                     (user-error "Unrecognized switch %S" (concat "--" (match-string 2 arg))))
                   (setf (alist-get (doom-cli-option-symbol opt) alist)
                         (or (if (doom-cli-option-args opt)
                                 (or (match-string 3 arg)
                                     (pop args)
                                     (user-error "%S expected an argument, but got none"
                                                 fullflag))
                               (if (match-string 3 arg)
                                   (user-error "%S was not expecting an argument, but got %S"
                                               fullflag (match-string 3 arg))
                                 fullflag))))))

                ((string-match "^\\(-\\([a-zA-Z0-9]+\\)\\)$" arg)
                 (let ((fullflag (match-string 1 arg))
                       (flag     (match-string 2 arg)))
                   (dolist (switch (split-string flag "" t))
                     (if-let (opt (doom--cli-get-option cli (concat "-" switch)))
                         (setf (alist-get (doom-cli-option-symbol opt) alist)
                               (if (doom-cli-option-args opt)
                                   (or (pop args)
                                       (user-error "%S expected an argument, but got none"
                                                   fullflag))
                                 fullflag))
                       (user-error "Unrecognized switch %S" (concat "-" switch))))))

                (arglist
                 (cl-incf got)
                 (let ((spec (pop arglist)))
                   (when (eq spec '&optional)
                     (setq spec (pop arglist)))
                   (setf (alist-get spec alist) arg))
                 (when (null arglist)
                   (throw 'done t)))

                (t
                 (push arg args)
                 (throw 'done t))))))
    (when (< got expected)
      (error "Expected %d arguments, got %d" expected got))
    (when rest
      (setf (alist-get restvar alist) rest))
    alist))

(defun doom-cli-get (command)
  "Return a CLI object associated by COMMAND name (string)."
  (cond ((null command) nil)
        ((doom-cli-p command) command)
        ((doom-cli-get
          (gethash (cond ((symbolp command) command)
                         ((stringp command) (intern command))
                         (command))
                   doom--cli-commands)))))

(defun doom-cli-internal-p (cli)
  "Return non-nil if CLI is an internal (non-public) command."
  (string-prefix-p ":" (doom-cli-name cli)))

(defun doom-cli-execute (command &rest args)
  "Execute COMMAND (string) with ARGS (list of strings).

Executes a cli defined with `defcli!' with the name or alias specified by
COMMAND, and passes ARGS to it."
  (if-let (cli (doom-cli-get command))
      (funcall (doom-cli-fn cli)
               (doom--cli-process cli (remq nil args)))
    (user-error "Couldn't find any %S command" command)))

(defmacro defcli! (name speclist &optional docstring &rest body)
  "Defines a CLI command.

COMMAND is a symbol or a list of symbols representing the aliases for this
command. DOCSTRING is a string description; its first line should be short
(under 60 characters), as it will be used as a summary for 'doom help'.

SPECLIST is a specification for options and arguments, which can be a list
specification for an option/switch in the following format:

  (VAR [FLAGS... ARGS...] DESCRIPTION)

Otherwise, SPECLIST accepts the same argument specifiers as `defun'.

BODY will be run when this dispatcher is called."
  (declare (indent 2) (doc-string 3))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring "TODO"))
  (let ((names (doom-enlist name))
        (optlist (cl-remove-if-not #'listp speclist))
        (arglist (cl-remove-if #'listp speclist))
        (plist (cl-loop for (key val) on body by #'cddr
                        if (keywordp key)
                        nconc (list key val) into plist
                        else return plist)))
    `(let ((name ',(car names))
           (aliases ',(cdr names))
           (plist ',plist))
       (when doom--cli-group
         (setq plist (plist-put plist :group doom--cli-group)))
       (puthash
        name
        (make-doom-cli (symbol-name name)
                       :desc ,docstring
                       :aliases (mapcar #'symbol-name aliases)
                       :arglist ',arglist
                       :optlist ',optlist
                       :plist plist
                       :fn
                       (lambda (--alist--)
                         (ignore --alist--)
                         (let ,(cl-loop for opt in speclist
                                        for optsym = (if (listp opt) (car opt) opt)
                                        unless (memq optsym cl--lambda-list-keywords)
                                        collect (list optsym `(cdr (assq ',optsym --alist--))))
                           ,@body)))
        doom--cli-commands)
       (when aliases
         (mapc (doom-rpartial #'puthash name doom--cli-commands)
               aliases)))))

(defmacro defcligroup! (name docstring &rest body)
  "Declare all enclosed cli commands are part of the NAME group."
  (declare (indent defun) (doc-string 2))
  `(let ((doom--cli-group ,name))
     (puthash doom--cli-group ,docstring doom--cli-groups)
     ,@body))


;;
;;; Debugger

(cl-defun doom-cli--debugger (error data)
  (cl-incf num-nonmacro-input-events)
  (cl-destructuring-bind (backtrace &optional type data . _)
      (cons (doom-cli--backtrace) data)
    (cond
     ((and (bound-and-true-p straight-process-buffer)
           (stringp data)
           (string-match-p (regexp-quote straight-process-buffer)
                           data))
      (print! (error "There was an unexpected package error"))
      (print-group!
       (print! "%s" (string-trim-right (straight--process-get-output)))))
     ((print! (error "There was an unexpected error"))
      (print-group!
       (print! "%s %s" (bold "Message:") (get type 'error-message))
       (print! "%s %S" (bold "Data:") (cons type data))
       (when backtrace
         (print! (bold "Backtrace:"))
         (print-group!
          (dolist (frame (seq-take backtrace 10))
            (print!
             "%0.74s" (replace-regexp-in-string
                       "[\n\r]" "\\\\n" (format "%S" frame)))))))))
    (when backtrace
      (with-temp-file doom-cli-log-error-file
        (insert "# -*- lisp-interaction -*-\n")
        (insert "# vim: set ft=lisp:\n")
        (let ((standard-output (current-buffer))
              (print-quoted t)
              (print-escape-newlines t)
              (print-escape-control-characters t)
              (print-level nil)
              (print-circle nil))
          (mapc #'print (cons (list type data) backtrace)))
        (print! (warn "Extended backtrace logged to %s")
                (relpath doom-cli-log-error-file)))))
  (throw 'exit 255))

(defun doom-cli--backtrace ()
  (let* ((n 0)
         (frame (backtrace-frame n))
         (frame-list nil)
         (in-program-stack nil))
    (while frame
      (when in-program-stack
        (push (cdr frame) frame-list))
      (when (eq (elt frame 1) 'doom-cli--debugger)
        (setq in-program-stack t))
      (when (and (eq (elt frame 1) 'doom-cli-execute)
                 (eq (elt frame 2) :doom))
        (setq in-program-stack nil))
      (setq n (1+ n)
            frame (backtrace-frame n)))
    (reverse frame-list)))


;;
;;; straight.el hacks

;; Straight was designed primarily for interactive use, in an interactive Emacs
;; session, but Doom does its package management in the terminal. Some things
;; must be modified get straight to behave and improve its UX for our users.

(defvar doom--straight-discard-options
  '(("has diverged from"
     . "^Reset [^ ]+ to branch")
    ("but recipe specifies a URL of"
     . "Delete remote \"[^\"]+\", re-create it with correct URL")
    ("has a merge conflict:"
     . "^Abort merge$")
    ("has a dirty worktree:"
     . "^Discard changes$")
    ("^In repository "
     . "^Reset branch \\|^Delete remote [^,]+, re-create it with correct URL"))
  "A list of regexps, mapped to regexps.

Their CAR is tested against the prompt, and CDR is tested against the presented
option, and is used by `straight-vc-git--popup-raw' to select which option to
recommend.

It may not be obvious to users what they should do for some straight prompts,
so Doom will recommend the one that reverts a package back to its (or target)
original state.")


;; HACK Remove dired & magit options from prompt, since they're inaccessible in
;;      noninteractive sessions.
(advice-add #'straight-vc-git--popup-raw :override #'straight--popup-raw)

;; HACK Replace GUI popup prompts (which hang indefinitely in tty Emacs) with
;;      simple prompts.
(defadvice! doom--straight-fallback-to-y-or-n-prompt-a (orig-fn &optional prompt)
  :around #'straight-are-you-sure
  (or doom-auto-accept
      (if doom-interactive-p
          (funcall orig-fn prompt)
        (y-or-n-p (format! "%s" (or prompt ""))))))

(defun doom--straight-recommended-option-p (prompt option)
  (cl-loop for (prompt-re . opt-re) in doom--straight-discard-options
           if (string-match-p prompt-re prompt)
           return (string-match-p opt-re option)))

(defadvice! doom--straight-fallback-to-tty-prompt-a (orig-fn prompt actions)
  "Modifies straight to prompt on the terminal when in noninteractive sessions."
  :around #'straight--popup-raw
  (if doom-interactive-p
      (funcall orig-fn prompt actions)
    (let ((doom--straight-discard-options doom--straight-discard-options))
      ;; We can't intercept C-g, so no point displaying any options for this key
      ;; when C-c is the proper way to abort batch Emacs.
      (delq! "C-g" actions 'assoc)
      ;; HACK These are associated with opening dired or magit, which isn't
      ;;      possible in tty Emacs, so...
      (delq! "e" actions 'assoc)
      (delq! "g" actions 'assoc)
      (if doom-auto-discard
          (cl-loop with doom-auto-accept = t
                   for (_key desc func) in actions
                   when desc
                   when (doom--straight-recommended-option-p prompt desc)
                   return (funcall func))
        (print! (start "%s") (red prompt))
        (print-group!
         (terpri)
         (let (options)
           (print-group!
            (print! " 1) Abort")
            (cl-loop for (_key desc func) in actions
                     when desc
                     do (push func options)
                     and do
                     (print! "%2s) %s" (1+ (length options))
                             (if (doom--straight-recommended-option-p prompt desc)
                                 (progn
                                   (setq doom--straight-discard-options nil)
                                   (green (concat desc " (Recommended)")))
                               desc))))
           (terpri)
           (let* ((options
                   (cons (lambda ()
                           (let ((doom-output-indent 0))
                             (terpri)
                             (print! (warn "Aborted")))
                           (kill-emacs 1))
                         (nreverse options)))
                  (prompt
                   (format! "How to proceed? (%s) "
                            (mapconcat #'number-to-string
                                       (number-sequence 1 (length options))
                                       ", ")))
                  answer fn)
             (while (null (nth (setq answer (1- (read-number prompt)))
                               options))
               (print! (warn "%s is not a valid answer, try again.")
                       answer))
             (funcall (nth answer options)))))))))

(defadvice! doom--straight-respect-print-indent-a (args)
  "Indent straight progress messages to respect `doom-output-indent', so we
don't have to pass whitespace to `straight-use-package's fourth argument
everywhere we use it (and internally)."
  :filter-args #'straight-use-package
  (cl-destructuring-bind
      (melpa-style-recipe &optional no-clone no-build cause interactive)
      args
    (list melpa-style-recipe no-clone no-build
          (if (and (not cause)
                   (boundp 'doom-output-indent)
                   (> doom-output-indent 0))
              (make-string (1- (or doom-output-indent 1)) 32)
            cause)
          interactive)))


;;
;;; Entry point

(defcli! :doom
    ((help-p        ["-h" "--help"]  "Same as help command")
     (auto-accept-p ["-y" "--yes"]   "Auto-accept all confirmation prompts")
     (debug-p       ["-d" "--debug"] "Enables on verbose output")
     (doomdir       ["--doomdir"  dir] "Use the private module at DIR (e.g. ~/.doom.d)")
     (localdir      ["--localdir" dir] "Use DIR as your local storage directory")
     &optional command
     &rest args)
  "A command line interface for managing Doom Emacs.

Includes package management, diagnostics, unit tests, and byte-compilation.

This tool also makes it trivial to launch Emacs out of a different folder or
with a different private module.

Environment variables:
  EMACSDIR      Where to find the Doom Emacs repo (normally ~/.emacs.d)
  DOOMDIR       Where to find your private Doom config (normally ~/.doom.d)
  DOOMLOCALDIR  Where to store local files (normally ~/.emacs.d/.local)"
  (condition-case e
      (with-output-to! doom-cli-log-file
        (catch 'exit
          (when (and (not (getenv "__DOOMRESTART"))
                     (or doomdir
                         localdir
                         debug-p
                         auto-accept-p))
            (when doomdir
              (setenv "DOOMDIR" (file-name-as-directory doomdir))
              (print! (info "DOOMDIR=%s") localdir))
            (when localdir
              (setenv "DOOMLOCALDIR" (file-name-as-directory localdir))
              (print! (info "DOOMLOCALDIR=%s") localdir))
            (when debug-p
              (setenv "DEBUG" "1")
              (print! (info "DEBUG=1")))
            (when auto-accept-p
              (setenv "YES" auto-accept-p)
              (print! (info "Confirmations auto-accept enabled")))
            (throw 'exit "__DOOMRESTART=1 $@"))
          ;; TODO Rotate logs out, instead of overwriting them?
          (delete-file doom-cli-log-file)
          (delete-file doom-cli-log-error-file)
          (when help-p
            (when command
              (push command args))
            (setq command "help"))
          (if (null command)
              (doom-cli-execute "help")
            (let ((start-time (current-time)))
              (run-hooks 'doom-cli-pre-hook)
              (when (apply #'doom-cli-execute command args)
                (run-hooks 'doom-cli-post-hook)
                (print! (success "Finished in %.4fs")
                        (float-time (time-subtract (current-time) start-time))))))))
    ;; TODO Not implemented yet
    (doom-cli-command-not-found-error
     (print! (error "Command 'doom %s' not recognized") (string-join (cdr e) " "))
     (print! "\nDid you mean one of these commands?")
     (apply #'doom-cli-execute "help" "--similar" (string-join (cdr e) " "))
     2)
    ;; TODO Not implemented yet
    (doom-cli-wrong-number-of-arguments-error
     (cl-destructuring-bind (route opt arg n d) (cdr e)
       (print! (error "doom %s: %S requires %d arguments, but %d given\n")
               (mapconcat #'symbol-name route " ") arg n d)
       (print-group!
        (apply #'doom-cli-execute "help" (mapcar #'symbol-name route))))
     3)
    ;; TODO Not implemented yet
    (doom-cli-unrecognized-option-error
     (let ((option (cadr e)))
       (print! (error "Unrecognized option: %S") option)
       (when (string-match "^--[^=]+=\\(.+\\)$" option)
         (print! "The %S syntax isn't supported. Use '%s %s' instead."
                 option (car (split-string option "="))
                 (match-string 1 option))))
     4)
    ;; TODO Not implemented yet
    (doom-cli-deprecated-error
     (cl-destructuring-bind (route . commands) (cdr e)
       (print! (warn "The 'doom %s' command was removed and replaced with:\n")
               (mapconcat #'symbol-name route " "))
       (print-group!
        (dolist (command commands)
          (print! (info "%s") command))))
     5)
    (user-error
     (print! (warn "%s") (cadr e))
     1)))


;;
;;; CLI Commands

(load! "cli/help")
(load! "cli/install")
(load! "cli/sync")
(load! "cli/env")
(load! "cli/upgrade")
(load! "cli/packages")
(load! "cli/autoloads")

(defcligroup! "Diagnostics"
  "For troubleshooting and diagnostics"
  (load! "cli/doctor")
  (load! "cli/debug")

  ;; Our tests are broken at the moment. Working on fixing them, but for now we
  ;; disable them:
  ;; (load! "cli/test")
  )

(defcligroup! "Compilation"
  "For compiling Doom and your config"
  (load! "cli/byte-compile"))

(defcligroup! "Utilities"
  "Conveniences for interacting with Doom externally"
  (defcli! run (&rest args)
    "Run Doom Emacs from bin/doom's parent directory.

All arguments are passed on to Emacs.

  doom run
  doom run -nw init.el

WARNING: this command exists for convenience and testing. Doom will suffer
additional overhead by being started this way. For the best performance, it is
best to run Doom out of ~/.emacs.d and ~/.doom.d."
    (throw 'exit (cons invocation-name args))))


;;
;;; Bootstrap

(doom-log "Initializing Doom CLI")
(load! doom-module-init-file doom-private-dir t)
(maphash (doom-module-loader doom-cli-file) doom-modules)
(load! doom-cli-file doom-private-dir t)

(provide 'core-cli)
;;; core-cli.el ends here
