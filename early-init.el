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

(let (file-name-handler-alist)
  ;; PERF: Garbage collection is a big contributor to startup times in both
  ;;   interactive and CLI sessions, so I defer it.
  (if noninteractive  ; in CLI sessions
      ;; PERF: GC deferral is less important in the CLI, but still helps script
      ;;   startup times. Just don't set it too high to avoid runaway memory
      ;;   usage in long-running elisp shell scripts.
      (setq gc-cons-threshold 134217728  ; 128mb
            ;; Backported from 29 (see emacs-mirror/emacs@73a384a98698)
            gc-cons-percentage 1.0)
    ;; PERF: Doom relies on `gcmh-mode' to reset this while the user is idle, so
    ;;   I effectively disable GC during startup. DON'T COPY THIS BLINDLY! If
    ;;   it's not reset later there will be stuttering, freezes, and crashes.
    (setq gc-cons-threshold most-positive-fixnum))

  ;; PERF: Don't use precious startup time to check mtimes on elisp bytecode.
  ;;   Ensuring correctness is 'doom sync's job. Although stale byte-code will
  ;;   heavily impact startup times, performance is unimportant when Emacs is in
  ;;   an error state.
  (setq load-prefer-newer noninteractive)

  ;; UX: Respect DEBUG envvar as an alternative to --debug-init, and to make
  ;;   startup more verbose sooner.
  (let ((debug (getenv-internal "DEBUG")))
    (when (stringp debug)
      (if (string-empty-p debug)
          (setenv "DEBUG" nil)
        (setq init-file-debug t
              debug-on-error t))))

  (let (;; FIX: Unset `command-line-args' in noninteractive sessions, to
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
      ;; Running 'doom sync' or 'doom profile sync --all' (re)generates a light
      ;; profile loader in $XDG_DATA_HOME/doom/profiles.X.el (or
      ;; $DOOMPROFILELOADFILE), after reading `doom-profile-load-path'. This
      ;; loader requires `$DOOMPROFILE' be set to function.
      (setenv "DOOMPROFILE" profile)
      (or (load (let ((windows? (memq system-type '(ms-dos windows-nt cygwin))))
                  (expand-file-name
                   (format (or (getenv-internal "DOOMPROFILELOADFILE")
                               (file-name-concat (if windows? "doomemacs/data" "doom")
                                                 "profiles.%d"))
                           emacs-major-version)
                   (or (if windows? (getenv-internal "LOCALAPPDATA"))
                       (getenv-internal "XDG_DATA_HOME")
                       "~/.local/share")))
                'noerror (not init-file-debug))
          (user-error "Profiles not initialized yet; run 'doom sync' first"))))

  ;; PERF: When `load'ing or `require'ing files, each permutation of
  ;;   `load-suffixes' and `load-file-rep-suffixes' (then `load-suffixes' +
  ;;   `load-file-rep-suffixes') is used to locate the file. Each permutation
  ;;   amounts to at least one file op, which is normally very fast, but can add
  ;;   up over the hundreds/thousands of files Emacs loads.
  ;;
  ;;   To reduce that burden -- and since Doom doesn't load any dynamic modules
  ;;   this early -- I remove `.so' from `load-suffixes' and pass the
  ;;   `must-suffix' arg to `load'. See the docs of `load' for details.
  (if (let ((load-suffixes '(".elc" ".el"))
            (doom (expand-file-name "lisp/doom" user-emacs-directory)))
        ;; I avoid `load's NOERROR argument because it suppresses other,
        ;; legitimate errors (like permission or IO errors), which gets
        ;; incorrectly interpreted as "this is not a Doom config".
        (if (file-exists-p (concat doom ".el"))
            ;; Load the heart of Doom Emacs.
            (load doom nil (not init-file-debug) nil 'must-suffix)
          ;; Failing that, assume we're loading a non-Doom config...
          ;; HACK: `startup--load-user-init-file' resolves $EMACSDIR from a
          ;;   lexical (and so, not-trivially-modifiable)
          ;;   `startup-init-directory', so Emacs will fail to locate the
          ;;   correct $EMACSDIR/init.el without help.
          (define-advice startup--load-user-init-file (:filter-args (args) reroute-to-profile)
            (list (lambda () (expand-file-name "init.el" user-emacs-directory))
                  nil (nth 2 args)))
          ;; (Re)set `user-init-file' for the `load' call further below, and do
          ;; so here while our `file-name-handler-alist' optimization is still
          ;; effective (benefits `expand-file-name'). BTW: Emacs resets
          ;; `user-init-file' and `early-init-file' after this file is loaded.
          (setq user-init-file (expand-file-name "early-init" user-emacs-directory))
          ;; COMPAT: I make no assumptions about the config we're going to load,
          ;;   so undo this file's global side-effects.
          (setq load-prefer-newer t)
          ;; PERF: But make an exception for `gc-cons-threshold', which I think
          ;;   all Emacs users and configs will benefit from. Still, setting it
          ;;   to `most-positive-fixnum' is dangerous if downstream does not
          ;;   reset it later to something reasonable, so I use 16mb as a best
          ;;   fit guess. It's better than Emacs' 80kb default.
          (setq gc-cons-threshold (* 16 1024 1024))
          nil))
      ;; Sets up Doom (particularly `doom-profile') for the session ahead. This
      ;; loads the profile's init file, if it's available. In interactive
      ;; session, a missing profile is an error state, in a non-interactive one,
      ;; it's not (and left to the consumer to deal with).
      (doom-initialize (not noninteractive))
    ;; If we're here, the user wants to load another config/profile (that may or
    ;; may not be a Doom config).
    (load user-init-file 'noerror (not init-file-debug) nil 'must-suffix)))

;;; early-init.el ends here
