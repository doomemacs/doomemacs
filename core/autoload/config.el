;;; core/autoload/config.el -*- lexical-binding: t; -*-

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
(defun doom/reload (&optional force-p)
  "Reloads your private config.

This is experimental! It will try to do as `bin/doom refresh' does, but from
within this Emacs session. i.e. it reload autoloads files (if necessary),
reloads your package list, and lastly, reloads your private config.el.

Runs `doom-reload-hook' afterwards."
  (interactive "P")
  (require 'core-cli)
  (general-auto-unbind-keys)
  (let ((doom-reloading-p t))
    (when (getenv "DOOMENV")
      (doom-reload-env-file 'force))
    (doom-reload-autoloads force-p)
    (let (doom-init-p)
      (doom-initialize))
    (with-demoted-errors "PRIVATE CONFIG ERROR: %s"
      (let (doom-init-modules-p)
        (doom-initialize-modules)))
    (when (bound-and-true-p doom-packages)
      (doom/reload-packages))
    (run-hook-wrapped 'doom-reload-hook #'doom-try-run-hook))
  (general-auto-unbind-keys t)
  (message "Finished!"))

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
  (doom-load-env-vars doom-env-file)
  (setq-default
   exec-path (append (split-string (getenv "PATH") ":")
                     (list exec-directory))
   shell-file-name (or (getenv "SHELL")
                       shell-file-name)))

;;;###autoload
(defun doom/reload-font ()
  "Reload your fonts, if they're set.
See `doom|init-fonts'."
  (interactive)
  (when doom-font
    (set-frame-font doom-font t))
  (doom|init-fonts)
  (mapc #'doom|init-emoji-fonts (frame-list)))

;;;###autoload
(defun doom/reload-theme ()
  "Reload the current color theme."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) doom-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (when (and doom-theme (not (memq doom-theme custom-enabled-themes)))
      (let (doom--prefer-theme-elc)
        (load-theme doom-theme t)))
    (doom|init-fonts)))
