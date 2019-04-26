;;; core/autoload/sessions.el -*- lexical-binding: t; -*-

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
  (cond ((require 'persp-mode nil t)
         (unless persp-mode
           (persp-mode +1))
         (persp-load-state-from-file file))
        ((and (require 'frameset nil t)
              (require 'restart-emacs nil t))
         (restart-emacs--restore-frames-using-desktop file))
        ((error "No session backend to load session with"))))


;;
;;; Command line switch

;;;###autoload
(defun doom-restore-session-handler (&rest _)
  "TODO"
  (add-hook 'window-setup-hook #'doom-load-session 'append))

;;;###autoload
(add-to-list 'command-switch-alist (cons "--restore" #'doom-restore-session-handler))


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
                               nil t
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Loading '%s' session" file)
  (doom-load-session file))

;;;###autoload
(defun doom/save-session (file)
  "TODO"
  (interactive
   (let ((session-file (doom-session-file)))
     (list (or (read-file-name "Save session to: "
                               (file-name-directory session-file)
                               nil nil
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  (message "Saving '%s' session" file)
  (doom-save-session file))

;;;###autoload
(defalias 'doom/restart #'restart-emacs)

;;;###autoload
(defun doom/restart-and-restore (&optional debug)
  "TODO"
  (interactive "P")
  (setq doom-autosave-session nil)
  (doom/quicksave-session)
  (restart-emacs
   (delq nil (list (if debug "--debug-init") "--restore"))))
