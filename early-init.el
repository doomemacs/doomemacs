;;; early-init.el --- Doom's universal bootstrapper -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This file, in summary:
;; - Determines where `user-emacs-directory' is by:
;;   - Processing `--init-directory DIR' (backported from Emacs 29),
;;   - Processing `--profile NAME' (see
;;     `https://docs.doomemacs.org/-/developers' or docs/developers.org),
;;   - Or assume that it's the directory this file lives in.
;; - Loads Doom as efficiently as possible, with only the essential startup
;;   optimizations, and prepares it for interactive or non-interactive sessions.
;; - If Doom isn't present, then we assume that Doom is being used as a
;;   bootloader and the user wants to load a non-Doom config, so we undo all our
;;   global side-effects, load `user-emacs-directory'/early-init.el, and carry
;;   on as normal (without Doom).
;; - Do all this without breaking compatibility with Chemacs.
;;
;; early-init.el was introduced in Emacs 27.1. It is loaded before init.el,
;; before Emacs initializes its UI or package.el, and before site files are
;; loaded. This is great place for startup optimizing, because only here can you
;; *prevent* things from loading, rather than turn them off after-the-fact.
;;
;; Doom uses this file as its "universal bootstrapper" for both interactive and
;; non-interactive sessions. That means: no matter what environment you want
;; Doom in, load this file first.
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
         ;; I avoid `load's NOERROR argument because other, legitimate errors
         ;; (like permission or IO errors) should not be suppressed or
         ;; interpreted as "this is not a Doom config".
         (condition-case _
             ;; Load the heart of Doom Emacs.
             (load (expand-file-name "lisp/doom" user-emacs-directory)
                   nil (not init-file-debug) nil 'must-suffix)
           ;; Failing that, assume that we're loading a non-Doom config.
           (file-missing
            ;; HACK: `startup--load-user-init-file' resolves $EMACSDIR from a
            ;;   lexically bound `startup-init-directory', which means changes
            ;;   to `user-emacs-directory' won't be respected when loading
            ;;   $EMACSDIR/init.el, so I force it to:
            (define-advice startup--load-user-init-file (:filter-args (args) reroute-to-profile)
              (list (lambda () (expand-file-name "init.el" user-emacs-directory))
                    nil (nth 2 args)))
            ;; Set `user-init-file' for the `load' call further below, and do so
            ;; here while our `file-name-handler-alist' optimization is still
            ;; effective (benefits `expand-file-name'). BTW: Emacs resets
            ;; `user-init-file' and `early-init-file' after this file is loaded.
            (setq user-init-file (expand-file-name "early-init" user-emacs-directory))
            ;; COMPAT: I make no assumptions about the config we're going to
            ;;   load, so undo this file's global side-effects.
            (setq load-prefer-newer t)
            ;; PERF: But make an exception for `gc-cons-threshold', which I
            ;;   think all Emacs users and configs will benefit from. Still,
            ;;   setting it to `most-positive-fixnum' is dangerous if downstream
            ;;   does not reset it later to something reasonable, so I use 16mb
            ;;   as a best fit guess. It's better than Emacs' 80kb default.
            (setq gc-cons-threshold (* 16 1024 1024))
            nil)))
       ;; ...But if Doom loaded then continue as normal.
       (doom-require (if noninteractive 'doom-cli 'doom-start))))

 ;; Then continue on to the config/profile we want to load.
 (load user-init-file 'noerror (not init-file-debug) nil 'must-suffix))

;;; early-init.el ends here
