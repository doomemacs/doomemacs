;;; core/cli/sync.el -*- lexical-binding: t; -*-

(defcli! (sync s)
  ((no-envvar-p ["-e"] "Don't regenerate the envvar file")
   (no-elc-p    ["-c"] "Don't recompile config")
   (update-p    ["-u"] "Update installed packages after syncing")
   (purge-p     ["-p" "--prune"] "Purge orphaned package repos & regraft them"))
  "Synchronize your config with Doom Emacs.

This is the equivalent of running autoremove, install, autoloads, then
recompile. Run this whenever you:

  1. Modify your `doom!' block,
  2. Add or remove `package!' blocks to your config,
  3. Add or remove autoloaded functions in module autoloaded files.
  4. Update Doom outside of Doom (e.g. with git)

It will ensure that unneeded packages are removed, all needed packages are
installed, autoloads files are up-to-date and no byte-compiled files have gone
stale."
  (run-hooks 'doom-sync-pre-hook)
  (add-hook 'kill-emacs-hook #'doom--cli-abort-warning-h)
  (print! (start "Synchronizing your config with Doom Emacs..."))
  (unwind-protect
      (print-group!
       (delete-file doom-autoloads-file)
       (when (and (not no-envvar-p)
                  (file-exists-p doom-env-file))
         (doom-cli-reload-env-file 'force))
       (doom-cli-packages-install)
       (doom-cli-packages-build)
       (when update-p
         (doom-cli-packages-update))
       (doom-cli-packages-purge purge-p 'builds-p purge-p purge-p purge-p)
       (run-hooks 'doom-sync-post-hook)
       (when (doom-autoloads-reload)
         (print! (info "Restart Emacs or use 'M-x doom/reload' for changes to take effect")))
       t)
    (remove-hook 'kill-emacs-hook #'doom--cli-abort-warning-h)))


;;
;;; DEPRECATED Commands

(defcli! (refresh re) ()
  "Deprecated for 'doom sync'"
  :hidden t
  (user-error "'doom refresh' has been replaced with 'doom sync'. Use that instead"))


;;
;;; Helpers

(defun doom--cli-abort-warning-h ()
  (terpri)
  (print! (warn "Script was abruptly aborted! Run 'doom sync' to repair inconsistencies")))
