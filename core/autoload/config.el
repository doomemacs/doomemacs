;;; core/autoload/config.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar doom-reload-hook nil
  "A list of hooks to run when `doom/reload' is called.")

;;;###autoload
(defvar doom-reloading-p nil
  "TODO")

;;;###autoload
(defun doom/open-private-config ()
  "Browse your `doom-private-dir'."
  (interactive)
  (unless (file-directory-p doom-private-dir)
    (make-directory doom-private-dir t))
  (doom-project-browse doom-private-dir))

;;;###autoload
(defun doom/find-file-in-private-config ()
  "Search for a file in `doom-private-dir'."
  (interactive)
  (doom-project-find-file doom-private-dir))

;;;###autoload
(defun doom/reload ()
  "Reloads your private config.

This is experimental! It will try to do as `bin/doom refresh' does, but from
within this Emacs session. i.e. it reload autoloads files (if necessary),
reloads your package list, and lastly, reloads your private config.el.

Runs `doom-reload-hook' afterwards."
  (interactive)
  (or (y-or-n-p
       (concat "You are about to reload your Doom config from within Emacs. This "
               "is highly experimental and may cause issues. It is recommended you "
               "use 'bin/doom refresh' on the command line instead.\n\n"
               "Reload anyway?"))
      (user-error "Aborted"))
  (require 'core-cli)
  (let ((doom-reloading-p t))
    (compile (format "%s/bin/doom refresh -f" doom-emacs-dir))
    (while compilation-in-progress
      (sit-for 1))
    (doom-initialize 'force)
    (with-demoted-errors "PRIVATE CONFIG ERROR: %s"
      (general-auto-unbind-keys)
      (unwind-protect
          (doom-initialize-modules 'force)
        (general-auto-unbind-keys t)))
    (run-hook-wrapped 'doom-reload-hook #'doom-try-run-hook))
  (message "Finished!"))

;;;###autoload
(defun doom/reload-autoloads ()
  "Reload only `doom-autoload-file' and `doom-package-autoload-file'.

This is much faster and safer than `doom/reload', but not as comprehensive. This
reloads your package and module visibility, but does not enable/disable It does
not reload your private config.

It is useful to only pull in changes performed by 'doom refresh' on the command
line."
  (interactive)
  (require 'core-cli)
  (require 'core-packages)
  (doom-initialize-packages)
  (doom-reload-autoloads nil 'force))

;;;###autoload
(defun doom/reload-env ()
  "Regenerates and reloads your shell environment.

Uses the same mechanism as 'bin/doom env reload'."
  (interactive)
  (compile (format "%s env refresh" (expand-file-name "bin/doom" doom-emacs-dir)))
  (while compilation-in-progress
    (sit-for 1))
  (unless (file-readable-p doom-env-file)
    (error "Failed to generate env file"))
  (doom-load-envvars-file doom-env-file))
