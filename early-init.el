;;; early-init.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; early-init.el was introduced in Emacs 27.1 and is loaded before init.el;
;; before Emacs initializes package.el and its UI; and before site files are
;; loaded. This is the best time to tweak Emacs (though any UI work will have to
;; be deferred).
;;
;; This file is responsible for bootstrapping an interactive session, and is
;; where all our dirtiest (and config-agnostic) startup hacks should live. It's
;; also home to Doom's bootloader, which lets you choose what Emacs config to
;; load with one of two switches:
;; - '--init-directory DIR' (backported from Emacs 29)
;; - Or Doom's profile system with '--profile NAME' (you declare configs in
;;   $EMACSDIR/profiles.el or implicitly as directories in $EMACSDIR/profiles/).
;;
;; You should *never* load this file in non-interactive sessions (e.g. batch
;; scripts). Load `doom-start' or use 'doom run' instead!
;;
;;; Code:

;; Garbage collection is a big contributor to startup times. This fends it off,
;; then is reset later by enabling `gcmh-mode'. Not resetting it will cause
;; stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; Prioritize old byte-compiled source files over newer sources. It saves us a
;; little IO time to skip all the mtime checks on each lookup.
(setq load-prefer-newer nil)

(unless (or (daemonp)
            init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun doom-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil :nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) init-doom)
    (advice-remove #'load-file #'load-file@silence)))


;;
;;; Detect `user-emacs-directory'

;; Prevent recursive profile processing, in case you're loading a Doom profile.
(unless (boundp 'doom-version)
  ;; Not using `command-switch-alist' to process --profile and --init-directory
  ;; was intentional. `command-switch-alist' is processed too late at startup to
  ;; change `user-emacs-directory' in time.

  ;; DEPRECATED Backported from 29. Remove this when 27/28 support is removed.
  (let ((initdir (or (cadr (member "--init-directory" command-line-args))
                     (getenv-internal "EMACSDIR"))))
    (when initdir
      ;; Discard the switch to prevent "invalid option" errors later.
      (add-to-list 'command-switch-alist (cons "--init-directory" (lambda (_) (pop argv))))
      (setq user-emacs-directory initdir)))

  (let ((profile (or (cadr (member "--profile" command-line-args))
                     (getenv-internal "DOOMPROFILE"))))
    (when profile
      ;; Discard the switch to prevent "invalid option" errors later.
      (add-to-list 'command-switch-alist (cons "--profile" (lambda (_) (pop argv))))
      ;; While processing the requested profile, Doom loosely expects
      ;; `user-emacs-directory' to be changed. If it doesn't, then you're using
      ;; profiles.el as a glorified, runtime dir-locals.el (which is fine, if
      ;; intended).
      (catch 'found
        (let ((profiles-file (expand-file-name "profiles.el" user-emacs-directory)))
          (when (file-exists-p profiles-file)
            (with-temp-buffer
              (let ((coding-system-for-read 'utf-8-auto))
                (insert-file-contents profiles-file))
              (condition-case-unless-debug e
                  (let ((profile-data (cdr (assq (intern profile) (read (current-buffer))))))
                    (dolist (var profile-data (if profile-data (throw 'found t)))
                      (if (eq (car var) 'env)
                          (dolist (env (cdr var)) (setenv (car env) (cdr env)))
                        (set (car var) (cdr var)))))
                (error (error "Failed to parse profiles.el: %s" (error-message-string e))))))
          ;; If the requested profile isn't in profiles.el, then see if
          ;; $EMACSDIR/profiles/$DOOMPROFILE exists. These are implicit
          ;; profiles, where `emacs --profile foo` will be equivalent to `emacs
          ;; --init-directory $EMACSDIR/profile/foo', if that directory exists.
          (let ((profile-dir
                 (expand-file-name
                  profile (or (getenv-internal "DOOMPROFILESDIR")
                              (expand-file-name "profiles/" user-emacs-directory)))))
            (when (file-directory-p profile-dir)
              (setq user-emacs-directory profile-dir)
              (throw 'found t)))

          (user-error "No %S profile found" profile)))

      (when init-file-debug
        (message "Selected profile: %s" profile))
      ;; Ensure the selected profile persists through the session
      (setenv "DOOMPROFILE" profile))))


;;
;;; Bootstrap

(let (init-file)
  ;; Load the heart of Doom Emacs
  (if (require 'doom (expand-file-name "lisp/doom" user-emacs-directory) t)
      ;; ...and prepare for an interactive session.
      (setq init-file (expand-file-name "doom-start" doom-core-dir))
    ;; ...but if that fails, then this is likely not a Doom config.
    (setq early-init-file (expand-file-name "early-init" user-emacs-directory))
    (load early-init-file t (not init-file-debug)))

  ;; We hijack Emacs' initfile resolver to inject our own entry point. Why do
  ;; this? Because:
  ;;
  ;; - It spares Emacs the effort of looking for/loading useless initfiles, like
  ;;   ~/.emacs and ~/_emacs. And skips ~/.emacs.d/init.el, which won't exist if
  ;;   you're using Doom (fyi: doom hackers or chemacs users could then use
  ;;   $EMACSDIR as their $DOOMDIR, if they wanted).
  ;; - Later, 'doom sync' will dynamically generate its bootstrap file, which
  ;;   will be important for Doom's profile system later. Until then, we'll use
  ;;   lisp/doom-start.el.
  ;; - A "fallback" initfile can be trivially specified, in case the
  ;;   bootstrapper is missing (if the user hasn't run 'doom sync' or is a
  ;;   first-timer). This is an opportunity to display a "safe mode" environment
  ;;   that's less intimidating and more helpful than the broken state errors
  ;;   would've left Emacs in, otherwise.
  ;; - A generated config allows for a file IO optimized startup.
  (define-advice startup--load-user-init-file (:filter-args (args) init-doom)
    "Initialize Doom Emacs in an interactive session."
    (list (lambda ()
            (or init-file
                (expand-file-name "init.el" user-emacs-directory)))
          (when (boundp 'doom-profiles-dir)
            (lambda ()
              (expand-file-name "safe-mode@static/init.el" doom-profiles-dir)))
          (caddr args))))

;;; early-init.el ends here
