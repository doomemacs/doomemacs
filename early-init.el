;;; early-init.el --- Doom's universal bootstrapper -*- lexical-binding: t -*-
;;; Commentary:
;;
;; early-init.el was introduced in Emacs 27.1 and is loaded before init.el, and
;; before Emacs initializes its UI or package.el, and before site files are
;; loaded. This is good place for startup optimizating, because only here can
;; you *prevent* things from loading, rather than turn them off after-the-fact.
;; As such, Doom does all its initializing here.
;;
;; This file is Doom's "universal bootstrapper" for both interactive and
;; non-interactive sessions. It's also the heart of its profile bootloader,
;; which allows you to switch between Emacs configs on demand using
;; `--init-directory DIR' (which was backported from Emacs 29) or `--profile
;; NAME` (more about profiles at `https://docs.doomemacs.org/-/developers' or
;; docs/developers.org).
;;
;; In summary, this file is responsible for:
;; - Setting up some universal startup optimizations.
;; - Determining where `user-emacs-directory' is from one of:
;;   - `--init-directory DIR' (backported from 29)
;;   - `--profile PROFILENAME'
;; - Do one of the following:
;;   - Load `doom' and one of `doom-start' or `doom-cli'.
;;   - Or (if the user is trying to load a non-Doom config) load
;;     `user-emacs-directory'/early-init.el.
;;
;;; Code:

;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but will be reset later by `gcmh-mode'. Not resetting it later will
;;   cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; PERF: Don't use precious startup time checking mtime on elisp bytecode.
;;   Ensuring correctness is 'doom sync's job, not the interactive session's.
;;   Still, stale byte-code will cause *heavy* losses in startup efficiency.
(setq load-prefer-newer noninteractive)

;; UX: Respect DEBUG envvar as an alternative to --debug-init, and to make are
;;   startup sufficiently verbose from this point on.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))


;;
;;; Bootstrap

(or
 ;; PERF: `file-name-handler-alist' is consulted often. Unsetting it offers a
 ;;   notable saving in startup time. This let-binding is just a stopgap though,
 ;;   a more complete version of this optimization can be found in lisp/doom.el.
 (let (file-name-handler-alist)
   (let* (;; FIX: Unset `command-line-args' in noninteractive sessions, to
          ;;   ensure upstream switches aren't misinterpreted.
          (command-line-args (unless noninteractive command-line-args))
          ;; I avoid using `command-switch-alist' to process --profile (and
          ;; --init-directory) because it is processed too late to change
          ;; `user-emacs-directory' in time.
          (profile (or (cadr (member "--profile" command-line-args))
                       (getenv-internal "DOOMPROFILE"))))
     (if (null profile)
         ;; REVIEW: Backported from Emacs 29. Remove when 28 support is dropped.
         (let ((init-dir (or (cadr (member "--init-directory" command-line-args))
                             (getenv-internal "EMACSDIR"))))
           (if (null init-dir)
               ;; FIX: If we've been loaded directly (via 'emacs -batch -l
               ;;   early-init.el') or by a doomscript (like bin/doom), and Doom
               ;;   is in a non-standard location (and/or Chemacs is used), then
               ;;   `user-emacs-directory' will be wrong.
               (when noninteractive
                 (setq user-emacs-directory
                       (file-name-directory (file-truename load-file-name))))
             ;; FIX: To prevent "invalid option" errors later.
             (push (cons "--init-directory" (lambda (_) (pop argv))) command-switch-alist)
             (setq user-emacs-directory (expand-file-name init-dir))))
       ;; FIX: Discard the switch to prevent "invalid option" errors later.
       (push (cons "--profile" (lambda (_) (pop argv))) command-switch-alist)
       ;; Running 'doom sync' or 'doom profile sync' (re)generates a light
       ;; profile loader in $EMACSDIR/profiles/load.el (or
       ;; $DOOMPROFILELOADFILE), after reading `doom-profile-load-path'. This
       ;; loader requires `$DOOMPROFILE' be set to function.
       (setenv "DOOMPROFILE" profile)
       (or (load (expand-file-name
                  (format (let ((lfile (getenv-internal "DOOMPROFILELOADFILE")))
                            (if lfile
                                (concat (string-remove-suffix ".el" lfile)
                                        ".%d.elc")
                              "profiles/load.%d.elc"))
                          emacs-major-version)
                  user-emacs-directory)
                 'noerror (not init-file-debug) 'nosuffix)
           (user-error "Profiles not initialized yet; run 'doom sync' first"))))

   ;; PERF: When `load'ing or `require'ing files, each permutation of
   ;;   `load-suffixes' and `load-file-rep-suffixes' (then `load-suffixes' +
   ;;   `load-file-rep-suffixes') is used to locate the file.  Each permutation
   ;;   is a file op, which is normally very fast, but they can add up over the
   ;;   hundreds/thousands of files Emacs needs to load.
   ;;
   ;;   To reduce that burden -- and since Doom doesn't load any dynamic modules
   ;;   -- I remove `.so' from `load-suffixes' and pass the `must-suffix' arg to
   ;;   `load'. See the docs of `load' for details.
   (if (let ((load-suffixes '(".elc" ".el")))
         ;; Load the heart of Doom Emacs.
         (load (expand-file-name "lisp/doom" user-emacs-directory)
               'noerror (not init-file-debug) nil 'must-suffix))
       ;; ...and prepare for the rest of the session.
       (doom-require (if noninteractive 'doom-cli 'doom-start))
     ;; Failing that, assume we're loading a non-Doom config and prepare.
     (setq user-init-file (expand-file-name "early-init" user-emacs-directory)
           ;; I make no assumptions about the config we're about to load, so
           ;; to limit side-effects, undo any leftover optimizations:
           load-prefer-newer t)
     nil))

 ;; Then continue on to the config/profile we want to load.
 (load early-init-file 'noerror (not init-file-debug) nil 'must-suffix))

;;; early-init.el ends here
