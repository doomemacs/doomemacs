;;; lisp/cli/sync.el --- synchronize config command -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load! "packages")


;;
;;; Variables

(defvar doom-after-sync-hook ()
  "Hooks run after 'doom sync' synchronizes the user's config with Doom.")

(defvar doom-before-sync-hook ()
  "Hooks run before 'doom sync' synchronizes the user's config with Doom.")

(defvar doom-cli-sync-info-file (file-name-concat doom-profile-data-dir "sync"))


;;
;;; Commands

(defcli! ((sync s))
    ((noenvvar? ("-e") "Don't regenerate the envvar file")
     (update?   ("-u") "Update all installed packages after syncing")
     (noupdate? ("-U") "Don't update any packages")
     (purge?    ("--gc") "Purge orphaned package repos & regraft them")
     (jobs      ("-j" "--jobs" num) "How many threads to use for native compilation")
     (rebuild?  ("-b" "--rebuild") "Rebuild, compile, & symlink installed packages")
     (auto?     ("-B") "Rebuild packages, but only if necessary")
     &context context)
  "Synchronize your config with Doom Emacs.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `doom!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files,
  4. Update Doom outside of Doom (e.g. with git),
  5. Move your Doom config (either $EMACSDIR or $DOOMDIR) to a new location.
  6. When you up (or down) grade Emacs itself.

It will ensure that unneeded packages are removed, all needed packages are
installed, autoloads files are up-to-date and no byte-compiled files have gone
stale.

OPTIONS:
  -j, --jobs
    Defaults to the maximum number of threads (or 1, if your CPU's threadcount
    can't be determined)."
  :benchmark t
  (when (doom-profiles-bootloadable-p)
    (call! '(profiles sync "--reload")))
  (when (doom-cli-context-suppress-prompts-p context)
    (setq auto? t))
  (when jobs
    (setq native-comp-async-jobs-number (truncate jobs)))
  (run-hooks 'doom-before-sync-hook)
  (add-hook 'kill-emacs-hook #'doom-sync--abort-warning-h)
  (print! (item "Using Emacs %s @ %s") emacs-version (path invocation-directory invocation-name))
  (print! (start "Synchronizing %S profile..." ) (or (car doom-profile) "default"))
  (unwind-protect
      (print-group!
        ;; If the user has up/downgraded Emacs since last sync, or copied their
        ;; config to a different system, then their packages need to be
        ;; recompiled. This is necessary because Emacs byte-code is not
        ;; necessarily back/forward compatible across major versions, and many
        ;; packages bake in hardcoded data at compile-time.
        (pcase-let ((`(,old-version . ,old-host) (doom-file-read doom-cli-sync-info-file :by 'read :noerror t))
                    (to-rebuild nil))
          (when (and old-version (not (equal old-version emacs-version)))
            (print! (warn "Emacs version has changed since last sync (from %s to %s)") old-version emacs-version)
            (setq to-rebuild t))
          (when (and old-host (not (equal old-host (system-name))))
            (print! (warn "Your system has changed since last sync"))
            (setq to-rebuild t))
          (when (and to-rebuild (not auto?))
            (or (y-or-n-p
                 (format! "  %s" "Your installed packages will need to be recompiled. Do so now?"))
                (exit! 0))
            (setq rebuild? t)))
        (when (and (not noenvvar?)
                   (file-exists-p
                    (file-name-concat doom-profile-dir
                                      doom-profile-env-file-name)))
          (call! '(env)))
        (doom-packages-ensure rebuild?)
        (unless noupdate? (doom-packages-update (not update?)))
        (doom-packages-purge purge? purge? purge? purge? purge?)
        (when (doom-profile-generate)
          (print! (item "Restart Emacs or use 'M-x doom/reload' for changes to take effect"))
          (run-hooks 'doom-after-sync-hook))
        (with-temp-file doom-cli-sync-info-file (prin1 (cons emacs-version (system-name)) (current-buffer)))
        t)
    (remove-hook 'kill-emacs-hook #'doom-sync--abort-warning-h)))


;;
;;; Helpers

(defun doom-sync--abort-warning-h ()
  (print! (warn "Script was abruptly aborted, leaving Doom in an incomplete state!"))
  (print! (item "Run 'doom sync' to repair it.")))

(provide 'doom-cli-sync)
;;; sync.el ends here
