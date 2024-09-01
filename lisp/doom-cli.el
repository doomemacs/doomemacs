;;; lisp/doom-cli.el --- API+DSL for Doom's CLI framework -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; The heart of Doom's CLI framework. This is safe to load in interactive
;; sessions (for API access and syntax highlighting), but much of the API
;; expects a noninteractive session, so take care when testing code!
;;
;;; Code:

(unless noninteractive
  (error "Don't load doom-cli in an interactive session!"))

;; PERF: Deferring the GC in non-interactive sessions isn't as important, but
;;   still yields a notable benefit. Still, avoid setting it to high here, as
;;   runaway memory usage is a real risk in longer sessions.
(setq gc-cons-threshold 134217728  ; 128mb
      ;; Backported from 29 (see emacs-mirror/emacs@73a384a98698)
      gc-cons-percentage 1.0)

;; REVIEW: Remove these later. The endpoints should be responsibile for
;;   ensuring they exist. For now, they exist to quell file errors.
(mapc (doom-rpartial #'make-directory 'parents)
      (list doom-local-dir
            doom-data-dir
            doom-cache-dir
            doom-state-dir))

;; HACK: bin/doom suppresses loading of site files so they can be loaded
;;   manually, here. Why? To suppress the otherwise unavoidable output they
;;   commonly produce (like deprecation notices, file-loaded messages, and
;;   linter warnings). This output pollutes the output of doom's CLI (or
;;   scripts derived from it) with potentially confusing or alarming -- but
;;   always unimportant -- information to the user.
(quiet!
 (require 'cl nil t)    ; "Package cl is deprecated"
 (unless site-run-file  ; unset in doom.el
   (when-let ((site-run-file (get 'site-run-file 'initial-value)))
     (load site-run-file t inhibit-message))))

(setq-default
 ;; PERF: Don't generate superfluous files when writing temp buffers.
 make-backup-files nil
 ;; COMPAT: Stop user configuration from interfering with package management.
 enable-dir-local-variables nil
 ;; PERF: Reduce ambiguity, embrace specificity, enjoy predictability.
 case-fold-search nil
 ;; UX: Don't clog the user's trash with our CLI refuse.
 delete-by-moving-to-trash nil)

;; Load just the... bear necessities~
(require 'seq)
(require 'map)

;; Suppress any possible coding system prompts during CLI sessions.
(set-language-environment "UTF-8")

;; Load and set up our debugger first, so backtraces can be made more
;; presentable and logged to file.
(doom-require 'doom-lib 'debug)
(if init-file-debug (doom-debug-mode +1))

;; Then load the rest of Doom's libs eagerly, since autoloads may not be
;; generated/loaded yet.
(doom-require 'doom-lib 'process)
(doom-require 'doom-lib 'system)
(doom-require 'doom-lib 'git)
(doom-require 'doom-lib 'plist)
(doom-require 'doom-lib 'files)
(doom-require 'doom-lib 'print)
(doom-require 'doom-lib 'autoloads)

;; Ensure straight and core packages are ready to go for CLI commands.
(require 'doom-cli-lib)
;; Last minute initialization at the end of loading this file.
(with-eval-after-load 'doom-cli
  (doom-run-hooks 'doom-before-init-hook))


;;
;;; Predefined CLIs (:help, :version, and :dump)

(defvar doom-help-commands '("%p %c {-?,--help}")
  "A list of help commands recognized for the running script.

Recognizes %p (for the prefix) and %c (for the active command).")

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

(defcli! (:root :help)
    ((localonly? ("-g" "--no-global") "Hide global options")
     (manpage?   ("--manpage")   "Generate in manpage format")
     (commands?  ("--commands")  "List all known commands")
     &multiple
     (sections   ("--synopsis" "--subcommands" "--similar" "--envvars"
                  "--postamble")
                 "Show only the specified sections.")
     &context context
     &args command)
  "Show documentation for a Doom CLI command.

OPTIONS:
  --synopsis, --subcommands, --similar, --envvars, --postamble
    TODO"
  (doom-cli-load-all)
  (when (doom-cli-context-error context)
    (terpri))
  (let* ((command (cons (doom-cli-context-prefix context) command))
         (cli (doom-cli-get command t))
         (rcli (doom-cli-get cli))
         (fallbackcli (cl-loop with targets = (doom-cli--command-expand (butlast command) t)
                               for cmd in (cons command targets)
                               if (doom-cli-get cmd t)
                               return it)))
    (cond (commands?
           (let ((cli (or cli (doom-cli-get (doom-cli-context-prefix context)))))
             (print! "Commands under '%s':\n%s"
                     (doom-cli-command-string cli)
                     (indent (doom-cli-help--render-commands
                              (or (doom-cli-subcommands cli)
                                  (user-error "No commands found"))
                              :prefix (doom-cli-command cli)
                              :inline? t
                              :docs? t)))))
          ((null sections)
           (if (null cli)
               (signal 'doom-cli-command-not-found-error command)
             (doom-cli-help--print cli context manpage? localonly?)
             (exit! :pager?)))
          ((dolist (section sections)
             (unless (equal section (car sections)) (terpri))
             (pcase section
               ("--synopsis"
                (print! "%s" (doom-cli-help--render-synopsis
                              (doom-cli-help--synopsis cli)
                              "Usage: ")))
               ("--subcommands"
                (print! "%s\n%s" (bold "Available commands:")
                        (indent (doom-cli-help--render-commands
                                 (doom-cli-subcommands rcli 1)
                                 :prefix command
                                 :grouped? t
                                 :docs? t)
                                doom-print-indent-increment)))
               ("--similar"
                (unless command
                  (user-error "No command specified"))
                (let ((similar (doom-cli-help-similar-commands command 0.4)))
                  (print! "Similar commands:")
                  (if (not similar)
                      (print! (indent (warn "Can't find any!")))
                    (dolist (command (seq-take similar 10))
                      (print! (indent (item "(%d%%) %s"))
                              (* (car command) 100)
                              (doom-cli-command-string (cdr command)))))))
               ("--envvars"
                (let* ((key "ENVIRONMENT VARIABLES")
                       (clis (if command (doom-cli-find command) (hash-table-values doom-cli--table)))
                       (clis (seq-remove #'doom-cli-alias clis))
                       (clis (seq-filter (fn! (cdr (assoc key (doom-cli-docs %)))) clis))
                       (clis (seq-group-by #'doom-cli-command clis)))
                  (print! "List of environment variables for %s:\n" command)
                  (if (null clis)
                      (print! (indent "None!"))
                    (dolist (group clis)
                      (print! (bold "%s%s:"
                                    (doom-cli-command-string (car group))
                                    (if (doom-cli-fn (doom-cli-get (car group)))
                                        "" " *")))
                      (dolist (cli (cdr group))
                        (print! (indent "%s") (markup (cdr (assoc key (doom-cli-docs cli))))))))))
               ("--postamble"
                (print! "See %s for documentation."
                        (join (cl-loop with spec =
                                       `((?p . ,(doom-cli-context-prefix context))
                                         (?c . ,(doom-cli-command-string (cdr (doom-cli-command (or cli fallbackcli))))))
                                       for cmd in doom-help-commands
                                       for formatted = (trim (format-spec cmd spec))
                                       collect (replace-regexp-in-string
                                                " +" " " (format "'%s'" formatted)))
                              " or ")))))))))

(defcli! (:root :version)
    ((simple? ("--simple"))
     &context context)
  "Show installed versions of Doom, Doom modules, and Emacs."
  (doom/version)
  (unless simple?
    (terpri)
    (with-temp-buffer
      (insert-file-contents (doom-path doom-emacs-dir "LICENSE"))
      (re-search-forward "^Copyright (c) ")
      (print! "%s\n" (trim (thing-at-point 'line t)))
      (print! (p "Doom Emacs uses the MIT license and is provided without warranty "
                 "of any kind. You may redistribute and modify copies if "
                 "given proper attribution. See the LICENSE file for details.")))))

(provide 'doom-cli)
;;; doom-cli.el ends here
