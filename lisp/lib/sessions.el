;;; lisp/lib/sessions.el -*- lexical-binding: t; -*-

(defvar desktop-base-file-name)
(defvar desktop-dirname)
(defvar desktop-restore-eager)
(defvar desktop-file-modtime)


;;
;;; Helpers

;;;###autoload
(defun doom-session-file (&optional name)
  "TODO"
  (cond ((require 'persp-mode nil t)
         (expand-file-name (or name persp-auto-save-fname) persp-save-dir))
        ((require 'desktop nil t)
         (if name
             (expand-file-name name (file-name-directory (desktop-full-file-name)))
           (desktop-full-file-name)))
        ((error "No session backend available"))))

;;;###autoload
(defun doom-save-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (doom-session-file))))
  (cond ((require 'persp-mode nil t)
         (unless persp-mode (persp-mode +1))
         (setq persp-auto-save-opt 0)
         (persp-save-state-to-file file))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (let ((frameset-filter-alist (append '((client . restart-emacs--record-tty-file))
                                              frameset-filter-alist))
               (desktop-base-file-name (file-name-nondirectory file))
               (desktop-dirname (file-name-directory file))
               (desktop-restore-eager t)
               desktop-file-modtime)
           (make-directory desktop-dirname t)
           ;; Prevents confirmation prompts
           (let ((desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))
             (desktop-save desktop-dirname t))))
        ((error "No session backend to save session with"))))

;;;###autoload
(defun doom-load-session (&optional file)
  "TODO"
  (setq file (expand-file-name (or file (doom-session-file))))
  (message "Attempting to load %s" file)
  (cond ((not (file-readable-p file))
         (message "No session file at %S to read from" file))
        ((require 'persp-mode nil t)
         (unless persp-mode
           (persp-mode +1))
         (let ((allowed (persp-list-persp-names-in-file file)))
           (cl-loop for name being the hash-keys of *persp-hash*
                    unless (member name allowed)
                    do (persp-kill name))
           (persp-load-state-from-file file)))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (restart-emacs--restore-frames-using-desktop file))
        ((error "No session backend to load session with"))))


;;
;;; Commands

;;;###autoload
(defun doom/quickload-session ()
  "TODO"
  (interactive)
  (message "Restoring session...")
  (doom-load-session)
  (message "Session restored. Welcome back."))

;;;###autoload
(defun doom/quicksave-session ()
  "TODO"
  (interactive)
  (message "Saving session")
  (doom-save-session)
  (message "Saving session...DONE"))

;;;###autoload
(defun doom/load-session (file)
  "TODO"
  (interactive
   (let ((session-file (doom-session-file)))
     (list (or (read-file-name "Session to restore: "
                               (file-name-directory session-file)
                               (file-name-nondirectory session-file)
                               t)
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Loading '%s' session" file)
  (doom-load-session file)
  (message "Session restored. Welcome back."))

;;;###autoload
(defun doom/save-session (file)
  "TODO"
  (interactive
   (let ((session-file (doom-session-file)))
     (list (or (read-file-name "Save session to: "
                               (file-name-directory session-file)
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Saving '%s' session" file)
  (doom-save-session file))

;;;###autoload
(defun doom/restart ()
  "Restart Emacs (and the daemon, if active).

Unlike `doom/restart-and-restore', does not restart the current session."
  (interactive)
  (require 'restart-emacs)
  (restart-emacs))

;;;###autoload
(defun doom/restart-and-restore (&optional debug)
  "Restart Emacs (and the daemon, if active).

If DEBUG (the prefix arg) is given, start the new instance with the --debug
switch."
  (interactive "P")
  (require 'restart-emacs)
  (doom/quicksave-session)
  (save-some-buffers nil t)
  (letf! ((#'save-buffers-kill-emacs #'kill-emacs)
          (confirm-kill-emacs)
          (tmpfile (make-temp-file "post-load")))
    ;; HACK `restart-emacs' does not properly escape arguments on Windows (in
    ;;   `restart-emacs--daemon-on-windows' and
    ;;   `restart-emacs--start-gui-on-windows'), so don't give it complex
    ;;   arguments at all. Should be fixed upstream, but restart-emacs seems to
    ;;   be unmaintained.
    (with-temp-file tmpfile
      (print `(progn
                (when (boundp 'doom-version)
                  (add-hook 'window-setup-hook #'doom-load-session 100))
                (delete-file ,tmpfile))
             (current-buffer)))
    (restart-emacs
     (append (if debug (list "--debug-init"))
             (when (boundp 'chemacs-current-emacs-profile)
               (list "--with-profile" chemacs-current-emacs-profile))
             (list "-l" tmpfile)))))
