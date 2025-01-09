;;; lisp/cli/sync.el --- synchronize config command -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(doom-require 'doom-lib 'packages)


;;
;;; Variables

(defvar doom-after-sync-hook ()
  "Hooks run after 'doom sync' synchronizes the user's config with Doom.")

(defvar doom-before-sync-hook ()
  "Hooks run before 'doom sync' synchronizes the user's config with Doom.")

;; DEPRECATED: Will be removed once `doom-profile' is a struct
(defvar doom-sync-info-file (file-name-concat doom-profile-data-dir "sync"))


;;
;;; Commands

(defcli! ((sync s))
    ((noenvvar? ("-e") "Don't regenerate the envvar file")
     (update?   ("-u") "Update all installed packages after syncing")
     (noupdate? ("-U") "Don't update any packages")
     (purge?    ("--gc") "Purge orphaned package repos & regraft them")
     (jobs      ("-j" "--jobs" num) "How many threads to use for native compilation")
     (rebuild?  ("-b" "--rebuild") "Rebuild all installed packages, unconditionally")
     (nobuild?  ("-B") "Don't rebuild packages when hostname or Emacs version has changed")
     (aot?      ("--aot") "Natively compile packages ahead-of-time (if available)")
     &context context)
  "Synchronize your config with Doom Emacs.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `doom!' block,
  2. Add, remove, or modify `package!' blocks to your config,
  3. Add, remove, or modify autoloaded functions in module autoloaded files,
  4. Update Doom manually (e.g. with git),
  5. Move your Doom config (either $EMACSDIR or $DOOMDIR) to a new location.
  6. Up or downgrade Emacs itself (e.g. 29.1 -> 29.4).

Sync'ing ensures that unneeded packages are removed, needed packages are
installed, autoloads files are up-to-date, and no byte-compiled files have gone
stale.

OPTIONS:
  -j, --jobs
    Defaults to the maximum number of threads (or 1, if your CPU's threadcount
    can't be determined).
  --aot
    Will only perform AOT native-compilation for packages updated/installed
    during the execution of this command. Use --rebuild as well to do so for all
    packages."
  :benchmark t
  (when aot?
    (after! straight
      (setq straight--native-comp-available t)))
  (when jobs
    (setq native-comp-async-jobs-number (truncate jobs)))
  (let ((emacs-running?
         (cl-loop for pid in (remove (emacs-pid) (list-system-processes))
                  for attrs = (process-attributes pid)
                  for args = (alist-get 'args attrs "")
                  if (string-match-p "^\\([^ ]+/\\)?[eE]macs" args)
                  if (not (string-match-p " --batch" args))
                  if (not (string-match-p " --script" args))
                  collect pid)))
    (when (doom-profiles-bootloadable-p)
      (call! '(profile sync "--all" "--reload")))
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
          (pcase-let ((`(,old-version . ,hash)
                       (doom-file-read doom-sync-info-file :by 'read :noerror t))
                      (to-rebuild nil))
            (when (and old-version (not (equal old-version emacs-version)))
              (print! (warn "Emacs version has changed since last sync (from %s to %s)") old-version emacs-version)
              (setq to-rebuild t))
            (when (and (stringp hash)
                       (not (equal hash (doom-sync--system-hash))))
              (print! (warn "Your system has changed since last sync"))
              (setq to-rebuild t))
            (when (and to-rebuild (not rebuild?) (not (doom-cli-context-suppress-prompts-p context)))
              (cond (nobuild?
                     (print! (warn "Packages must be rebuilt, but -B has prevented it. Skipping...")))
                    ((doom-cli-context-get context 'upgrading)
                     (print! (warn "Packages will be rebuilt"))
                     (setq rebuild? t))
                    ((y-or-n-p (format! "  %s" "Installed packages must be rebuilt. Do so now?"))
                     (setq rebuild? t))
                    ((exit! 0)))))
          (when (and (not noenvvar?)
                     (file-exists-p doom-env-file))
            (call! '(env)))
          (doom-packages-ensure rebuild?)
          (unless noupdate? (doom-packages-update (not update?)))
          (doom-packages-purge purge? purge? purge? purge? purge?)
          (when (doom-profile-generate)
            (when emacs-running?
              (print! (item "Restart Emacs for changes to take effect")))
            (run-hooks 'doom-after-sync-hook))
          (when (or rebuild? (not (file-exists-p doom-sync-info-file)))
            (with-temp-file doom-sync-info-file
              (prin1 (cons emacs-version (doom-sync--system-hash))
                     (current-buffer))))
          t)
      (remove-hook 'kill-emacs-hook #'doom-sync--abort-warning-h))))


;;
;;; Helpers

(defun doom-sync--system-hash ()
  (secure-hash
   'md5 (mapconcat
         #'identity
         (list
          ;; Changes to this path could indicate a change to the username and/or
          ;; the location of Straight's build artifacts; both warrant a rebuild
          ;; of your packages.
          doom-local-dir
          ;; Changes to this indicate the user's system/OS has changed (e.g. if
          ;; the user copied their config to another system, on another OS) or
          ;; Emacs' compiled features have (even if the major version hasn't).
          system-configuration)
         "")))

(defun doom-sync--abort-warning-h ()
  (print! (warn "Script was abruptly aborted, leaving Doom in an incomplete state!"))
  (print! (item "Run 'doom sync' to repair it.")))

(provide 'doom-cli-sync)
;;; sync.el ends here
