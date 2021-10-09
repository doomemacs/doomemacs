;;; core/core-cli.el --- -*- lexical-binding: t; no-byte-compile: t; -*-

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
  "Where to write the extended output to.")

(defvar doom-cli-log-error-file (concat doom-local-dir "doom.error.log")
  "Where to write the last backtrace to.")

(defvar doom--cli-log-buffer (generate-new-buffer " *doom log*"))
(defvar doom--cli-commands (make-hash-table :test 'equal))
(defvar doom--cli-groups (make-hash-table :test 'equal))
(defvar doom--cli-group nil)

(define-error 'doom-cli-error "There was an unexpected error" 'doom-error)
(define-error 'doom-cli-command-not-found-error "Could not find that command" 'doom-cli-error)
(define-error 'doom-cli-wrong-number-of-arguments-error "Wrong number of CLI arguments" 'doom-cli-error)
(define-error 'doom-cli-unrecognized-option-error "Not a recognized option" 'doom-cli-error)
(define-error 'doom-cli-deprecated-error "Command is deprecated" 'doom-cli-error)


;;
;;; Bootstrap

(require 'seq)
(load! "autoload/process")
(load! "autoload/system")
(load! "autoload/plist")
(load! "autoload/files")
(load! "autoload/output")

(load! "cli/lib/debugger")
(load! "cli/lib/lib")
(load! "cli/lib/straight-hacks")

;; Use our own home-grown debugger so we can capture and store backtraces, make
;; them more presentable, and make it easier for users to produce better bug
;; reports!
(setq debugger #'doom-cli--debugger
      debug-on-error t
      debug-ignored-errors '(user-error))

;; Create all our core directories to quell file errors.
(mapc (doom-rpartial #'make-directory 'parents)
      (list doom-local-dir
            doom-etc-dir
            doom-cache-dir))

;; Ensure straight and core packages are ready to go for CLI commands.
(require 'core-modules)
(require 'core-packages)
(doom-initialize-core-packages)

;; Default to using all cores, rather than half of them, since we compile things
;; ahead-of-time in a non-interactive session.
(defadvice! doom--comp-use-all-cores-a (&rest _)
  :before #'comp-effective-async-max-jobs
  (setq comp-num-cpus (doom-system-cpus)))


;;
;;; Entry point

(defcli! :doom
    ((help-p        ["-h" "--help"]  "Same as help command")
     (auto-accept-p ["-y" "--yes"]   "Auto-accept all confirmation prompts")
     (debug-p       ["-d" "--debug"] "Enables on verbose output")
     (loadfile      ["-l" "--load" file] "Load an elisp FILE before executing any commands")
     (doomdir       ["--doomdir"  dir] "Use the private module at DIR (e.g. ~/.doom.d)")
     (localdir      ["--localdir" dir] "Use DIR as your local storage directory")
     (nocolor       ["-C" "--nocolor"] "Disable colored output")
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
      (with-output-to! doom--cli-log-buffer
        (when nocolor
          (setq doom-output-backend nil))
        (catch 'exit
          (when (and (not (getenv "__DOOMRESTART"))
                     (or doomdir
                         localdir
                         debug-p
                         auto-accept-p))
            (when doomdir
              (setenv "DOOMDIR" (file-name-as-directory doomdir))
              (print! (info "DOOMDIR=%s") doomdir))
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
          (when loadfile
            (load (doom-path loadfile) nil t t))
          (when help-p
            (when command
              (push command args))
            (setq command "help"))
          (cons
           t (if (null command)
                 (doom-cli-execute "help")
               (let ((start-time (current-time)))
                 (run-hooks 'doom-cli-pre-hook)
                 (unless (getenv "__DOOMRESTART")
                   (print! (start "Executing 'doom %s' with Emacs %s at %s")
                           (string-join
                            (cons (or (ignore-errors
                                        (doom-cli-name (doom-cli-get command)))
                                      command)
                                  args)
                            " ")
                           emacs-version
                           (format-time-string "%Y-%m-%d %H:%M:%S")))
                 (print-group!
                  (when-let (result (apply #'doom-cli-execute command args))
                    (run-hooks 'doom-cli-post-hook)
                    (print! (success "Finished in %s")
                            (let* ((duration (float-time (time-subtract (current-time) before-init-time)))
                                   (hours   (/ (truncate duration) 60 60))
                                   (minutes (- (/ (truncate duration) 60) (* hours 60)))
                                   (seconds (- duration (* hours 60 60) (* minutes 60))))
                              (string-join
                               (delq
                                nil (list (unless (zerop hours)   (format "%dh" hours))
                                          (unless (zerop minutes) (format "%dm" minutes))
                                          (format (if (> duration 60) "%ds" "%.4fs")
                                                  seconds))))))
                    result)))))))
    ;; TODO Not implemented yet
    (doom-cli-command-not-found-error
     (print! (error "Command 'doom %s' not recognized") (string-join (cdr e) " "))
     (print! "\nDid you mean one of these commands?")
     (apply #'doom-cli-execute "help" "--similar" (string-join (cdr e) " "))
     5)
    ;; TODO Not implemented yet
    (doom-cli-wrong-number-of-arguments-error
     (cl-destructuring-bind (route opt arg n d) (cdr e)
       (print! (error "doom %s: %S requires %d arguments, but %d given\n")
               (mapconcat #'symbol-name route " ") arg n d)
       (print-group!
        (apply #'doom-cli-execute "help" (mapcar #'symbol-name route))))
     6)
    ;; TODO Not implemented yet
    (doom-cli-unrecognized-option-error
     (let ((option (cadr e)))
       (print! (error "Unrecognized option: %S") option)
       (when (string-match "^--[^=]+=\\(.+\\)$" option)
         (print! "The %S syntax isn't supported. Use '%s %s' instead."
                 option (car (split-string option "="))
                 (match-string 1 option))))
     7)
    ;; TODO Not implemented yet
    (doom-cli-deprecated-error
     (cl-destructuring-bind (route . commands) (cdr e)
       (print! (warn "The 'doom %s' command was removed and replaced with:\n")
               (mapconcat #'symbol-name route " "))
       (print-group!
        (dolist (command commands)
          (print! (info "%s") command))))
     8)
    (user-error
     (print! (warn "%s") (cadr e))
     9)))


;;
;;; CLI Commands

(load! "cli/help")
(load! "cli/install")
(load! "cli/sync")
(load! "cli/env")
(load! "cli/upgrade")
(load! "cli/packages")
(load! "cli/autoloads")
(load! "cli/ci")

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


;; Don't generate superfluous files when writing temp buffers
(setq make-backup-files nil)
;; Stop user configuration from interfering with package management
(setq enable-dir-local-variables nil)
;; Reduce ambiguity, embrace specificity. It's more predictable.
(setq-default case-fold-search nil)
;; Don't clog the user's trash with anything we clean up in this session.
(setq delete-by-moving-to-trash nil)

(provide 'core-cli)
;;; core-cli.el ends here
