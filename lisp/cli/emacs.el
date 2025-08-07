;;; lisp/cli/emacs.el --- launching Emacs and sandboxes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'doom)
(require 'doom-cli)

;;
;;; Variables

;; None yet!


;;
;;; Commands

(defcli! emacs
    (;; TODO Implement sandbox functionality post-3.0
     ;; (daemon?     ("--daemon"))
     ;; (window-type ("--gui" "--tty"))
     ;; (version     ("--with-emacs" version))
     ;; (doomversion ("--with-doom" version))
     ;; (profile     ("--profile" name))
     (repl?  ("--repl") "Launch an elisp REPL")
     ;; &multiple
     ;; (calls  ("-f" "--funcall" fn))
     ;; (loads  ("-l" "--load" file))
     ;; (evals  (     "--eval" form))
     &context context
     &input input
     &rest args)
  "Launch Doom Emacs or an Emacs sandbox

Opens from bin/doom's parent directory.

Keep in mind there is some overhead opening Doom this way. For the best
performance, it is best to run Doom out of ~/.config/emacs or ~/.emacs.d."
  :benchmark nil
  (if repl?
      (if input
          ;; Evaluate piped-in text directly, if given.
          (eval (read input) t)
        (doom-emacs-repl context))
    (let* ((tempdir  (doom-path (temporary-file-directory) "doom.run"))
           (tempemacsdir (doom-path tempdir ".emacs.d")))
      (delete-directory tempdir t) ; start from scratch
      (make-directory tempemacsdir t)
      ;; HACK: So that Emacs doesn't lose track of local state, like user fonts,
      ;;   configs, or binscripts, we symlink these to the sandbox.
      ;; REVIEW: Use `--init-directory' when we drop 29 support OR when Doom is
      ;;   in bootloader mode.
      (dolist (dir (list (cons "XDG_DATA_HOME"   ".local/share")
                         (cons "XDG_STATE_HOME"  ".local/state")
                         (cons "XDG_BIN_HOME"    ".local/bin")
                         (cons "XDG_CONFIG_HOME" ".config")
                         (cons "XDG_CACHE_HOME"  ".cache")))
        (let* ((source (expand-file-name (or (getenv (car dir)) (expand-file-name (cdr dir) "~"))))
               (target (expand-file-name (cdr dir) tempdir)))
          (when (file-directory-p source)
            (unless (file-symlink-p target)
              (make-directory (file-name-directory target) t)
              (make-symbolic-link source target)))))
      (with-temp-file (doom-path tempemacsdir "early-init.el")
        (prin1 `(progn
                  ;; Restore sane values for these envvars
                  (setenv "HOME" ,(getenv "HOME"))
                  (setenv "EMACSDIR" ,doom-emacs-dir)
                  (setenv "DOOMDIR"  ,doom-user-dir)
                  (setenv "DOOMPROFILE" ,(getenv "DOOMPROFILE"))
                  (setq early-init-file ,(doom-path doom-emacs-dir "early-init.el"))
                  (load early-init-file nil (not ,init-file-debug) 'nosuffix))
               (current-buffer)))
      (exit! (format "HOME=%S %s %s"
                     tempdir
                     invocation-name
                     (combine-and-quote-strings args))))))


;;
;;; Helpers

(defun doom-emacs-repl (_context)
  "Launch a rudimentary Elisp REPL."
  ;; I wrote this for fun; not with any serious intention of adding a
  ;; fully-fledged REPL to the Doom CLI. Still, I occasionally need to check
  ;; something, and once this has nix integration and can sandbox Emacs versions
  ;; separately, it may be useful for quick tests and demos.
  (let (form)
    (while (setq form (read-from-minibuffer "(elisp) $ "))
      (when (member form '(":quit" ":q"))
        (print! "\nGoodbye!")
        (exit! 0))
      (let (debug-on-error)
        (condition-case e
            (print! "%S" (eval (read form) t))
          (error
           (let* ((n 0)
                  (frame (backtrace-frame n))
                  (frame-list nil)
                  (in-program-stack t))
             (while frame
               (when in-program-stack
                 (push (cdr frame) frame-list))
               ;; (when (eq (elt frame 1) 'doom-run-repl)
               ;;   (setq in-program-stack t))
               (when (eq (elt frame 1) 'doom-run-repl)
                 (setq in-program-stack nil))
               (setq n (1+ n)
                     frame (backtrace-frame n)))
             (let* (;; (depth doom-cli-log-backtrace-depth)
                    (print-escape-newlines t))
               (print! (error "There was an unexpected error"))
               (print-group!
                 (print! "%s %s" (bold "Message:") (error-message-string e))
                 (print! "%s %S" (bold "Details:") (cdr e))))))))
      (terpri))))

(provide 'doom-cli '(emacs))
;;; emacs.el ends here
