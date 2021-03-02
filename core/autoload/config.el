;;; core/autoload/config.el -*- lexical-binding: t; -*-

(defvar doom-bin-dir (concat doom-emacs-dir "bin/"))
(defvar doom-bin (concat doom-bin-dir "doom"))

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
(defun doom/goto-private-init-file ()
  "Open your private init.el file.
And jumps to your `doom!' block."
  (interactive)
  (find-file (expand-file-name "init.el" doom-private-dir))
  (goto-char
   (or (save-excursion
         (goto-char (point-min))
         (search-forward "(doom!" nil t))
       (point))))

;;;###autoload
(defun doom/goto-private-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name "config.el" doom-private-dir)))

;;;###autoload
(defun doom/goto-private-packages-file ()
  "Open your private packages.el file."
  (interactive)
  (find-file (expand-file-name "packages.el" doom-private-dir)))


;;
;;; Managements

(defmacro doom--if-compile (command on-success &optional on-failure)
  (declare (indent 2))
  `(with-current-buffer (compile ,command t)
     (let ((w (get-buffer-window (current-buffer))))
       (select-window w)
       (add-hook
        'compilation-finish-functions
        (lambda (_buf status)
          (if (equal status "finished\n")
              (progn
                (delete-window w)
                ,on-success)
            ,on-failure))
        nil 'local))))

;;;###autoload
(defun doom/reload ()
  "Reloads your private config.

This is experimental! It will try to do as `bin/doom sync' does, but from within
this Emacs session. i.e. it reload autoloads files (if necessary), reloads your
package list, and lastly, reloads your private config.el.

Runs `doom-after-reload-hook' afterwards."
  (interactive)
  (require 'core-cli)
  (when (and IS-WINDOWS (file-exists-p doom-env-file))
    (message "Can't regenerate envvar file from within Emacs. Run 'doom env' from the console"))
  ;; In case doom/reload is run before incrementally loaded packages are loaded,
  ;; which could cause odd load order issues.
  (mapc #'require (cdr doom-incremental-packages))
  (doom--if-compile (format "%S sync -e" doom-bin)
      (let ((doom-reloading-p t))
        (run-hook-wrapped 'doom-before-reload-hook #'doom-try-run-hook)
        (doom-initialize 'force)
        (with-demoted-errors "PRIVATE CONFIG ERROR: %s"
          (general-auto-unbind-keys)
          (unwind-protect
              (doom-initialize-modules 'force)
            (general-auto-unbind-keys t)))
        (run-hook-wrapped 'doom-after-reload-hook #'doom-try-run-hook)
        (message "Config successfully reloaded!"))
    (user-error "Failed to reload your config")))

;;;###autoload
(defun doom/reload-autoloads ()
  "Reload only `doom-autoloads-file' and `doom-package-autoload-file'.

This is much faster and safer than `doom/reload', but not as comprehensive. This
reloads your package and module visibility, but does not install new packages or
remove orphaned ones. It also doesn't reload your private config.

It is useful to only pull in changes performed by 'doom sync' on the command
line."
  (interactive)
  (require 'core-cli)
  (require 'core-packages)
  (doom-initialize-packages)
  (doom-autoloads-reload))

;;;###autoload
(defun doom/reload-env (&optional arg)
  "Regenerates and/or reloads your envvar file.

If passed the prefix ARG, clear the envvar file. Uses the same mechanism as
'bin/doom env'.

An envvar file contains a snapshot of your shell environment, which can be
imported into Emacs."
  (interactive "P")
  (when IS-WINDOWS
    (user-error "Cannot reload envvar file from within Emacs on Windows, run it from cmd.exe"))
  (doom--if-compile
      (format "%s -ic '%S env%s'"
              (string-trim
               (shell-command-to-string
                (format "getent passwd %S | cut -d: -f7"
                        (user-login-name))))
              doom-bin (if arg " -c" ""))
      (let ((doom-reloading-p t))
        (unless arg
          (doom-load-envvars-file doom-env-file)))
    (error "Failed to generate env file")))

;;;###autoload
(defun doom/upgrade ()
  "Run 'doom upgrade' then prompt to restart Emacs."
  (interactive)
  (doom--if-compile (format "%S -y upgrade" doom-bin)
      (when (y-or-n-p "You must restart Emacs for the upgrade to take effect.\n\nRestart Emacs?")
        (doom/restart-and-restore))))
