;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;; Eagerly load these libraries because this module may be loaded in a session
;; that hasn't been fully initialized (where autoloads files haven't been
;; generated or `load-path' populated).
(load! "autoload/debug")
(load! "autoload/files")
(load! "autoload/message")
(load! "autoload/packages")


;;
;; Dispatcher API

(defvar doom-auto-accept (getenv "YES")
  "If non-nil, Doom will auto-accept any confirmation prompts during batch
commands like `doom-packages-install', `doom-packages-update' and
`doom-packages-autoremove'.")

(defconst doom--dispatch-command-alist ())
(defconst doom--dispatch-alias-alist ())

(defun doom--dispatch-format (desc &optional short)
  (with-temp-buffer
    (let ((fill-column 72))
      (insert desc)
      (goto-char (point-min))
      (while (re-search-forward "\n\n[^ \n]" nil t)
        (fill-paragraph)))
    (if (not short)
        (buffer-string)
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun doom--dispatch-help (&optional command desc &rest args)
  "Display help documentation for a dispatcher command. If COMMAND and DESC are
omitted, show all available commands, their aliases and brief descriptions."
  (if command
      (princ (doom--dispatch-format desc))
    (print! (bold "%-10s\t%s\t%s") "Command:" "Alias" "Description")
    (dolist (spec (cl-sort doom--dispatch-command-alist #'string-lessp
                           :key #'car))
      (cl-destructuring-bind (command &key desc _body) spec
        (let ((aliases (cl-loop for (alias . cmd) in doom--dispatch-alias-alist
                                if (eq cmd command)
                                collect (symbol-name alias))))
          (print! "  %-10s\t%s\t%s"
                  command (if aliases (string-join aliases ",") "")
                  (doom--dispatch-format desc t)))))))

(defun doom-dispatch (cmd args &optional show-help)
  "Parses ARGS and invokes a dispatcher.

If SHOW-HELP is non-nil, show the documentation for said dispatcher."
  (when (equal cmd "help")
    (setq show-help t)
    (when args
      (setq cmd  (car args)
            args (cdr args))))
  (cl-destructuring-bind (command &key desc body)
      (let ((sym (intern cmd)))
        (or (assq sym doom--dispatch-command-alist)
            (assq (cdr (assq sym doom--dispatch-alias-alist))
                  doom--dispatch-command-alist)
            (user-error "Invalid command: %s" sym)))
    (if show-help
        (apply #'doom--dispatch-help command desc args)
      (funcall body args))))

(defmacro dispatcher! (command form &optional docstring)
  "Define a dispatcher command. COMMAND is a symbol or a list of symbols
representing the aliases for this command. DESC is a string description. The
first line should be short (under 60 letters), as it will be displayed for
bin/doom help.

BODY will be run when this dispatcher is called."
  (declare (indent defun) (doc-string 3))
  (cl-destructuring-bind (cmd &rest aliases)
      (doom-enlist command)
    (macroexp-progn
     (append
      (when aliases
        `((dolist (alias ',aliases)
            (setf (alist-get alias doom--dispatch-alias-alist) ',cmd))))
      `((setf (alist-get ',cmd doom--dispatch-command-alist)
              (list :desc ,docstring
                    :body (lambda (args) (ignore args) ,form))))))))


;;
;; Dummy dispatch commands (no-op because they're handled especially)

(dispatcher! run :noop
  "Run Doom Emacs from bin/doom's parent directory.

All arguments are passed on to Emacs (except for -p and -e).

  doom run
  doom run -nw init.el

WARNING: this command exists for convenience and testing. Doom will suffer
additional overhead by being started this way. For the best performance, it is
best to run Doom out of ~/.emacs.d and ~/.doom.d.")

(dispatcher! (doctor doc) :noop
  "Checks for issues with your environment & Doom config.

Also checks for missing dependencies for any enabled modules.")

(dispatcher! (help h) :noop
  "Look up additional information about a command.")


;;
;; Real dispatch commands

(load! "cli/autoloads")
(load! "cli/byte-compile")
(load! "cli/debug")
(load! "cli/packages")
(load! "cli/patch-macos")
(load! "cli/quickstart")
(load! "cli/upgrade")
(load! "cli/test")


;;
(defun doom-refresh (&optional force-p)
  "Ensure Doom is in a working state by checking autoloads and packages, and
recompiling any changed compiled files. This is the shotgun solution to most
problems with doom."
  (doom-reload-doom-autoloads force-p)
  (unwind-protect
      (progn
        (ignore-errors
          (doom-packages-autoremove doom-auto-accept))
        (ignore-errors
          (doom-packages-install doom-auto-accept)))
    (doom-reload-package-autoloads force-p)
    (doom-byte-compile nil 'recompile)))

(dispatcher! (refresh re) (doom-refresh 'force)
  "Refresh Doom. Same as autoremove+install+autoloads.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `doom!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files.
  4. Update Doom outside of Doom (e.g. with git)")

(provide 'core-cli)
;;; core-cli.el ends here
