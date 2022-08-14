;;; lisp/doom-cli.el --- The heart of Doom's CLI framework -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; The bootstrapper for Doom's CLI. This is *not* safe to load in interactive
;; sessions as it has many side-effects. Loads `doom-cli-lib' instead for API
;; access and syntax highlighting.
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
(setq gc-cons-threshold 134217728   ; 128mb
      gc-cons-percentage 1.0)

;; Ensure errors are sufficiently detailed from this point on.
(setq debug-on-error t)
;; Be more verbose if debug mode is on.
(when (setq init-file-debug (getenv "DEBUG"))
  (message "Debug mode enabled"))

;;; Initialize profile
(let ((profile (getenv "DOOMPROFILE")))
  (when profile
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-auto)
            (profiles-file (expand-file-name "profiles.el" user-emacs-directory)))
        (condition-case e
            (progn
              (insert-file-contents profiles-file)
              (dolist (var (or (cdr (assq (intern profile) (read (current-buffer))))
                               (progn (message "No %S profile found" profile)
                                      (kill-emacs 3))))
                (if (eq (car var) 'env)
                    (dolist (env (cdr var)) (setenv (car env) (cdr env)))
                  (set (car var) (cdr var)))))
          (file-missing
           (message "No $EMACSDIR/%s file to look up %S in."
                    (file-name-nondirectory profiles-file)
                    profile)
           (kill-emacs 3))
          (end-of-file (signal 'end-of-file (list profiles-file)))
          (error (error "Parser error in profiles.el: %s" (error-message-string e))))))))

;; HACK Load `cl' and site files manually to prevent polluting logs and stdout
;;      with deprecation and/or file load messages.
(let ((inhibit-message (not init-file-debug)))
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
(require 'doom (expand-file-name "doom" (file-name-directory load-file-name)))

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
            doom-data-dir
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

(provide 'doom-cli)
;;; doom-cli.el ends here
