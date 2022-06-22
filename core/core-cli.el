;;; core/core-cli.el --- The heart of Doom's CLI framework -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; The heart of Doom's CLI framework. This is safe to load in interactive
;; sessions (for API access and syntax highlighting), but much of the API
;; expects a noninteractive session, so take care when testing code!
;;
;;; Code:

(when (version< emacs-version "27.1")
  (message
   (concat
    "Error: detected Emacs " emacs-version ", but 27.1 or newer is required.\n\n"
    "The version of Emacs in use is located at:\n\n  " (car command-line-args) "\n\n"
    "A guide for installing a newer version of Emacs can be found at:\n\n  "
    (format "https://doomemacs.org/docs/getting_started.org#%s\n"
            (cond ((eq system-type 'darwin) "on-macos")
                  ((memq system-type '(cygwin windows-nt ms-dos)) "on-windows")
                  ("on-linux"))) "\n"
    "Alternatively, alter the EMACS environment variable to temporarily change what\n"
    "command this script uses to invoke Emacs. For example:\n\n"
    (let ((command (file-name-nondirectory (cadr (member "--load" command-line-args)))))
      (concat "  $ EMACS=/path/to/valid/emacs " command " ...\n"
              "  $ EMACS=\"/Applications/Emacs.app/Contents/MacOS/Emacs\" " command " ...\n"
              "  $ EMACS=\"snap run emacs\" " command " ...\n"))
    "\nAborting..."))
  (kill-emacs 2))


;;
;;; Setup CLI session

;; The garbage collector isn't so important during CLI ops. A higher threshold
;; makes it 15-30% faster, but set it too high and we risk runaway memory usage
;; in longer sessions.
(setq gc-cons-threshold 134217728)   ; 128mb

;; Ensure errors are sufficiently detailed from this point on.
(setq debug-on-error t)

;; HACK Load `cl' and site files manually to prevent polluting logs and stdout
;;      with deprecation and/or file load messages.
(let ((inhibit-message (not (or (getenv "DEBUG") init-file-debug))))
  (require 'cl)
  (unless site-run-file
    (let ((site-run-file "site-start")
          (tail load-path)
          (lispdir (expand-file-name "../lisp" data-directory))
          dir)
      (while tail
        (setq dir (car tail))
        (let ((default-directory dir))
          (load (expand-file-name "subdirs.el") t inhibit-message t))
        (unless (string-prefix-p lispdir dir)
          (let ((default-directory dir))
            (load (expand-file-name "leim-list.el") t inhibit-message t)))
        (setq tail (cdr tail)))
      (load site-run-file t inhibit-message))))

;; Just the... bear necessities~
(require 'core (expand-file-name "core" (file-name-directory load-file-name)))

;; Load these eagerly, since autoloads haven't been generated/loaded yet
(load! "autoload/process")
(load! "autoload/system")
(load! "autoload/plist")
(load! "autoload/files")
(load! "autoload/debug")
(load! "autoload/print")
;; (load! "autoload/autoloads")

;; Ensure straight and core packages are ready to go for CLI commands.
(require 'core-modules)
(require 'core-packages)

;; Our DSL, API, and everything nice.
(require 'core-cli-lib)

;; Don't generate superfluous files when writing temp buffers.
(setq make-backup-files nil)
;; Stop user configuration from interfering with package management.
(setq enable-dir-local-variables nil)
;; Reduce ambiguity, embrace specificity, enjoy predictability.
(setq-default case-fold-search nil)
;; Don't clog the user's trash with anything we clean up during this session.
(setq delete-by-moving-to-trash nil)


;;
;;; Bootstrap

;; Use our own home-grown debugger so we can capture backtraces, make them more
;; presentable, and write them to a file. Cleaner backtraces are better UX than
;; the giant wall of text the default debugger throws up.
(setq debugger #'doom-cli-debugger)

;; Create all our core directories to quell file errors.
(mapc (doom-rpartial #'make-directory 'parents)
      (list doom-local-dir
            doom-etc-dir
            doom-cache-dir))

;; Load standard :help and :version handlers.
(load! "cli/help")

;; When __DOOMDUMP is set, doomscripts trigger this special handler.
(defcli! (:root :dump)
    ((pretty? ("--pretty") "Pretty print output")
     &context context
     &args commands)
  "Dump metadata to stdout for other commands to read."
  (let* ((prefix (doom-cli-context-prefix context))
         (command (cons prefix commands)))
    (funcall (if pretty? #'pp #'prin1)
             (cond ((equal commands '("-")) (hash-table-values doom-cli--table))
                   (commands (doom-cli-find command))
                   ((doom-cli-find (list prefix)))))
    (terpri)
    ;; Kill manually so we don't save output to logs.
    (let (kill-emacs) (kill-emacs 0))))

(provide 'core-cli)
;;; core-cli.el ends here
