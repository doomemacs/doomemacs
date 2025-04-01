;;; lisp/lib/config.el -*- lexical-binding: t; -*-

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
    (user-error "$DOOMDIR doesn't exist (%s)" (abbreviate-file-name doom-user-dir)))
  (doom-project-browse doom-user-dir))

;;;###autoload
(defun doom/find-file-in-private-config ()
  "Search for a file in `doom-user-dir'."
  (interactive)
  (doom-project-find-file doom-user-dir))


;;
;;; Managements

(defmacro doom--if-compile (command on-success &optional on-failure)
  (declare (indent 2))
  `(let* ((doom-bin "doom")
          (doom-bin-dir (expand-file-name "bin/" doom-emacs-dir))
          (default-directory doom-emacs-dir)
          (exec-path (cons doom-bin-dir exec-path)))
     (when (and (featurep :system 'windows)
                (string-match-p "cmdproxy.exe$" shell-file-name))
       (unless (executable-find "pwsh")
         (user-error "Powershell 3.0+ is required, but pwsh.exe was not found in your $PATH"))
       (setq doom-bin "doom.ps1"))
     ;; Ensure the bin/doom operates with the same environment as this
     ;; running session.
     (with-environment-variables
         (("PATH" (string-join exec-path path-separator))
          ("EMACS" (doom-path invocation-directory invocation-name))
          ("EMACSDIR" doom-emacs-dir)
          ("DOOMDIR" doom-user-dir)
          ("DOOMLOCALDIR" doom-local-dir)
          ("DEBUG" (if doom-debug-mode (number-to-string doom-log-level))))
       (with-current-buffer
           (compile (format ,command (expand-file-name doom-bin doom-bin-dir)) t)
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
            nil 'local))))))

(defvar doom-reload-command
  (format "%s sync -B -e"
          ;; /usr/bin/env doesn't exist on Android
          (if (featurep :system 'android)
              "sh %s"
            "%s"))
  "Command that `doom/reload' runs.")
;;;###autoload
(defun doom/reload ()
  "Reloads your private config.

WARNING: This command is experimental! If you haven't configured your config to
be idempotent, then this could cause compounding slowness or errors.

This is experimental! It will try to do as `bin/doom sync' does, but from within
this Emacs session. i.e. it reload autoloads files (if necessary), reloads your
package list, and lastly, reloads your private config.el.

Runs `doom-after-reload-hook' afterwards."
  (interactive)
  (mapc #'require (cdr doom-incremental-packages))
  (doom--if-compile doom-reload-command
      (with-doom-context 'reload
        (doom-run-hooks 'doom-before-reload-hook)
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
  (doom-require 'doom-lib 'profiles)
  ;; TODO: Make this more robust
  (with-doom-context 'reload
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
  (with-doom-context 'reload
    (let ((default-directory doom-emacs-dir))
      (with-temp-buffer
        (doom-load-envvars-file doom-env-file)
        (message "Reloaded %S" (abbreviate-file-name doom-env-file))))))

(defvar doom-upgrade-command
  (format "%s upgrade -B --force"
          ;; /usr/bin/env doesn't exist on Android
          (if (featurep :system 'android)
              "sh %s"
            "%s"))
  "Command that `doom/upgrade' runs.")
;;;###autoload
(defun doom/upgrade ()
  "Run 'doom upgrade' then prompt to restart Emacs."
  (interactive)
  (doom--if-compile doom-upgrade-command
      (when (y-or-n-p "You must restart Emacs for the upgrade to take effect.\n\nRestart Emacs?")
        (doom/restart-and-restore))))

(provide 'doom-lib '(config))
;;; config.el ends here
