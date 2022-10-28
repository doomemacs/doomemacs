;;; lisp/lib/config.el -*- lexical-binding: t; -*-

(defvar doom-bin-dir (expand-file-name "bin/" doom-emacs-dir))
(defvar doom-bin (expand-file-name "doom" doom-bin-dir))

;;;###autoload
(defvar doom-after-reload-hook nil
  "A list of hooks to run after `doom/reload' has reloaded Doom.")

;;;###autoload
(defvar doom-before-reload-hook nil
  "A list of hooks to run before `doom/reload' has reloaded Doom.")

;;;###autoload
(defun doom/open-private-config ()
  "Browse your `doom-user-dir'."
  (interactive)
  (unless (file-directory-p doom-user-dir)
    (make-directory doom-user-dir t))
  (doom-project-browse doom-user-dir))

;;;###autoload
(defun doom/find-file-in-private-config ()
  "Search for a file in `doom-user-dir'."
  (interactive)
  (doom-project-find-file doom-user-dir))

;;;###autoload
(defun doom/goto-private-init-file ()
  "Open your private init.el file.
And jumps to your `doom!' block."
  (interactive)
  (find-file (expand-file-name doom-module-init-file doom-user-dir))
  (goto-char
   (or (save-excursion
         (goto-char (point-min))
         (search-forward "(doom!" nil t))
       (point))))

;;;###autoload
(defun doom/goto-private-config-file ()
  "Open your private config.el file."
  (interactive)
  (find-file (expand-file-name doom-module-config-file doom-user-dir)))

;;;###autoload
(defun doom/goto-private-packages-file ()
  "Open your private packages.el file."
  (interactive)
  (find-file (expand-file-name doom-module-packages-file doom-user-dir)))


;;
;;; Managements

(defmacro doom--if-compile (command on-success &optional on-failure)
  (declare (indent 2))
  `(let ((default-directory doom-emacs-dir))
     (with-current-buffer (compile ,command t)
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
          nil 'local)))))

;;;###autoload
(defun doom/reload ()
  "Reloads your private config.

This is experimental! It will try to do as `bin/doom sync' does, but from within
this Emacs session. i.e. it reload autoloads files (if necessary), reloads your
package list, and lastly, reloads your private config.el.

Runs `doom-after-reload-hook' afterwards."
  (interactive)
  (mapc #'require (cdr doom-incremental-packages))
  (doom--if-compile (format "%S sync -e" doom-bin)
      (doom-context-with '(reload modules)
        (doom-run-hooks 'doom-before-reload-hook)
        (doom-load (file-name-concat doom-user-dir doom-module-init-file) t)
        (with-demoted-errors "PRIVATE CONFIG ERROR: %s"
          (general-auto-unbind-keys)
          (unwind-protect
              (startup--load-user-init-file nil)
            (general-auto-unbind-keys t)))
        (doom-run-hooks 'doom-after-reload-hook)
        (message "Config successfully reloaded!"))
    (user-error "Failed to reload your config")))

;;;###autoload
(defun doom/reload-autoloads ()
  "Reload only the autoloads of the current profile.

This is much faster and safer than `doom/reload', but not as comprehensive. This
reloads your package and module visibility, but does not install new packages or
remove orphaned ones. It also doesn't reload your private config.

It is useful to only pull in changes performed by 'doom sync' on the command
line."
  (interactive)
  (require 'doom-profiles)
  ;; TODO: Make this more robust
  (doom-context-with 'reload
    (dolist (file (mapcar #'car doom-profile-generators))
      (when (string-match-p "/[0-9]+-loaddefs[.-]" file)
        (load (doom-path doom-profile-dir doom-profile-init-dir-name file)
              'noerror)))))

;;;###autoload
(defun doom/reload-env ()
  "Reloads your envvar file.

DOES NOT REGENERATE IT. You must run 'doom env' in your shell OUTSIDE of Emacs.
Doing so from within Emacs will taint your shell environment.

An envvar file contains a snapshot of your shell environment, which can be
imported into Emacs."
  (interactive)
  (doom-context-with 'reload
    (let ((default-directory doom-emacs-dir))
      (with-temp-buffer
        (doom-load-envvars-file doom-env-file)
        (message "Reloaded %S" (abbreviate-file-name doom-env-file))))))

;;;###autoload
(defun doom/upgrade ()
  "Run 'doom upgrade' then prompt to restart Emacs."
  (interactive)
  (doom--if-compile (format "%S upgrade --force" doom-bin)
      (when (y-or-n-p "You must restart Emacs for the upgrade to take effect.\n\nRestart Emacs?")
        (doom/restart-and-restore))))
