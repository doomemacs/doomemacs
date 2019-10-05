;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'seq)


(defvar doom-auto-accept (getenv "YES")
  "If non-nil, Doom will auto-accept any confirmation prompts during batch
commands like `doom-packages-install', `doom-packages-update' and
`doom-packages-autoremove'.")

(defvar doom-cli-pre-execute-hook nil
  "TODO")
(defvar doom-cli-post-success-execute-hook nil
  "TODO")

(defvar doom--cli-commands (make-hash-table :test 'equal))
(defvar doom--cli-groups (make-hash-table :test 'equal))
(defvar doom--cli-group nil)


;;
;;; Dispatcher API

(defun doom-sh (command &rest args)
  "Execute COMMAND with ARGS in the shell and return (STATUS . OUTPUT).

STATUS is a boolean"
  (let ((output (get-buffer-create "*doom-sh-output*")))
    (unwind-protect
        (cons (or (apply #'call-process command nil output nil args)
                  -1)
              (with-current-buffer output
                (string-trim (buffer-string))))
      (kill-buffer output))))

(defun doom--dispatch-command (command)
  (when (symbolp command)
    (setq command (symbol-name command)))
  (cl-check-type command string)
  (intern-soft
   (format "doom-cli-%s"
           (if (gethash command doom--cli-commands)
               command
             (cl-loop for key
                      being the hash-keys in doom--cli-commands
                      for aliases = (plist-get (gethash key doom--cli-commands) :aliases)
                      if (member command aliases)
                      return key)))))

(defun doom--dispatch-format (desc &optional short)
  (with-temp-buffer
    (let ((fill-column 72))
      (save-excursion
        (insert desc)
        (while (re-search-backward "\n\n[^ \n]" nil t)
          (fill-paragraph))))
    (if (not short)
        (buffer-string)
      (buffer-substring (line-beginning-position)
                        (line-end-position)))))

(defun doom--dispatch-help-1 (command)
  (cl-destructuring-bind (&key aliases hidden _group)
      (gethash command doom--cli-commands)
    (unless hidden
      (print! "%-11s\t%s\t%s"
              command (if aliases (string-join aliases ",") "")
              (doom--dispatch-format
               (documentation (doom--dispatch-command command))
               t)))))

(defun doom--dispatch-help (&optional fn &rest args)
  "Display help documentation for a dispatcher command. If fn and DESC are
omitted, show all available commands, their aliases and brief descriptions."
  (if fn
      (princ (documentation fn))
    (print! (bold "%-11s\t%s\t%s" "Command:" "Alias" "Description"))
    (print-group!
     (dolist (group (seq-group-by (lambda (key) (plist-get (gethash key doom--cli-commands) :group))
                                  (hash-table-keys doom--cli-commands)))
       (if (null (car group))
           (mapc #'doom--dispatch-help-1 (cdr group))
         (print! "%-30s\t%s" (bold (car group)) (gethash (car group) doom--cli-groups))
         (print-group!
          (mapc #'doom--dispatch-help-1 (cdr group))))
       (terpri)))))

(defun doom-dispatch (cmd args &optional show-help)
  "Parses ARGS and invokes a dispatcher.

If SHOW-HELP is non-nil, show the documentation for said dispatcher."
  (when (equal cmd "help")
    (setq show-help t)
    (when args
      (setq cmd  (car args)
            args (cdr args))))
  (let ((fn (doom--dispatch-command cmd)))
    (unless (fboundp fn)
      (user-error "%S is not any command *I* know!" cmd))
    (if show-help
        (doom--dispatch-help fn args)
      (let ((start-time (current-time)))
        (run-hooks 'doom-cli-pre-execute-hook)
        (unwind-protect
            (when-let (ret (apply fn args))
              (print!
               "\n%s"
               (success "Finished! (%.4fs)"
                        (float-time
                         (time-subtract (current-time)
                                        start-time))))
              (run-hooks 'doom-cli-post-execute-hook)
              ret)
          (run-hooks 'doom-cli-post-error-execute-hook))))))

(defmacro defcligroup! (name docstring &rest body)
  "TODO"
  (declare (indent defun) (doc-string 2))
  `(let ((doom--cli-group ,name))
     (puthash doom--cli-group ,docstring doom--cli-groups)
     ,@body))

(defmacro defcli! (names arglist docstring &rest body)
  "Define a dispatcher command. COMMAND is a symbol or a list of symbols
representing the aliases for this command. DESC is a string description. The
first line should be short (under 60 letters), as it will be displayed for
bin/doom help.

BODY will be run when this dispatcher is called."
  (declare (indent defun) (doc-string 3))
  (let* ((names (mapcar #'symbol-name (doom-enlist names)))
         (fn (intern (format "doom-cli-%s" (car names))))
         (plist (cl-loop while (keywordp (car body))
                         collect (pop body)
                         collect (pop body))))
    (macroexp-progn
     (reverse
      `((let ((plist ',plist))
          (setq plist (plist-put plist :aliases ',(cdr names)))
          (unless (or (plist-member plist :group)
                      (null doom--cli-group))
            (plist-put plist :group doom--cli-group))
          (puthash ,(car names) plist doom--cli-commands))
        (defun ,fn ,arglist
          ,docstring
          ,@body))))))


;;
;;; Dispatch commands

;; Load all of our subcommands
(defcli! (refresh re) (&rest args)
  "Ensure Doom is properly set up.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `doom!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files.
  4. Update Doom outside of Doom (e.g. with git)

It will ensure that unneeded packages are removed, all needed packages are
installed, autoloads files are up-to-date and no byte-compiled files have gone
stale."
  (print! (green "Initiating a refresh of Doom Emacs...\n"))
  (let ((force-p (or (member "-f" args)
                     (member "--force" args)))
        success)
    (when (file-exists-p doom-env-file)
      (doom-reload-env-file 'force))
    (doom-reload-core-autoloads force-p)
    (unwind-protect
        (progn
          (and (doom-packages-install doom-auto-accept)
               (setq success t))
          (and (doom-packages-rebuild doom-auto-accept)
               (setq success t))
          (and (doom-packages-purge nil 'builds-p nil doom-auto-accept)
               (setq success t)))
      (doom-reload-package-autoloads (or success force-p))
      (doom-byte-compile nil 'recompile))
    t))


;; Load all of our subcommands
(load! "cli/install")

(defcligroup! "Diagnostics"
  "For troubleshooting and diagnostics"
  (defcli! (doctor doc) ()
    "Checks for issues with your environment & Doom config.

Use the doctor to diagnose common problems or list missing dependencies in
enabled modules.")

  (load! "cli/debug")
  (load! "cli/test"))

(defcligroup! "Maintenance"
  "For managing your config and packages"
  (load! "cli/env")
  (load! "cli/upgrade")
  (load! "cli/packages")
  (load! "cli/autoloads"))

(defcligroup! "Byte compilation"
  "For byte-compiling Doom and your config"
  (load! "cli/byte-compile"))

(defcligroup! "Utilities"
  "Conveniences for interacting with Doom externally"
  (defcli! run ()
    "Run Doom Emacs from bin/doom's parent directory.

All arguments are passed on to Emacs (except for -p and -e).

  doom run
  doom run -nw init.el

WARNING: this command exists for convenience and testing. Doom will suffer
additional overhead by being started this way. For the best performance, it is
best to run Doom out of ~/.emacs.d and ~/.doom.d.")

  ;; (load! "cli/batch")
  ;; (load! "cli/org")
  )

(provide 'core-cli)
;;; core-cli.el ends here
