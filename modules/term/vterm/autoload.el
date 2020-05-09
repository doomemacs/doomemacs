;;; term/vterm/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vterm/toggle (arg)
  "Toggles a terminal popup window at project root.

If prefix ARG is non-nil, recreate vterm buffer in the current project's root."
  (interactive "P")
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (let ((buffer-name
         (format "*doom:vterm-popup:%s*"
                 (if (bound-and-true-p persp-mode)
                     (safe-persp-name (get-current-persp))
                   "main")))
        confirm-kill-processes
        current-prefix-arg)
    (when arg
      (let ((buffer (get-buffer buffer-name))
            (window (get-buffer-window buffer-name)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (window-live-p window)
          (delete-window window))))
    (if-let (win (get-buffer-window buffer-name))
        (if (eq (selected-window) win)
            (delete-window win)
          (select-window win)
          (when (bound-and-true-p evil-local-mode)
            (evil-change-to-initial-state))
          (goto-char (point-max)))
      (setenv "PROOT" (or (doom-project-root) default-directory))
      (let ((buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (unless (eq major-mode 'vterm-mode)
            (vterm-mode))
          (+vterm--change-directory-if-remote))
        (pop-to-buffer buffer)))))

;;;###autoload
(defun +vterm/here (arg)
  "Open a terminal buffer in the current window at project root.

If prefix ARG is non-nil, cd into `default-directory' instead of project root."
  (interactive "P")
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (require 'vterm)
  ;; This hack forces vterm to redraw, fixing strange artefacting in the tty.
  (save-window-excursion
    (pop-to-buffer "*scratch*"))
  (let* ((project-root (or (doom-project-root) default-directory))
         (default-directory
           (if arg
               default-directory
             project-root))
         display-buffer-alist)
    (setenv "PROOT" project-root)
    (vterm)
    (+vterm--change-directory-if-remote)))

(defun +vterm--change-directory-if-remote ()
  "When `default-directory` is remote, use the corresponding
method to prepare vterm at the corresponding remote directory."
  (when (and (featurep 'tramp)
             (tramp-tramp-file-p default-directory))
    (message "default-directory is %s" default-directory)
    (with-parsed-tramp-file-name default-directory path
      (let ((method (cadr (assoc `tramp-login-program
                                 (assoc path-method tramp-methods)))))
        (vterm-send-string
         (concat method " "
                 (when path-user (concat path-user "@")) path-host))
        (vterm-send-return)
        (vterm-send-string
         (concat "cd " path-localname))
        (vterm-send-return)))))


(defvar +vterm--insert-point nil)

;;;###autoload
(defun +vterm-remember-insert-point-h ()
  "Remember point when leaving insert mode."
  (setq-local +vterm--insert-point (point)))

;;;###autoload
(defun +vterm-goto-insert-point-h ()
  "Go to the point we were at when we left insert mode."
  (when +vterm--insert-point
    (goto-char +vterm--insert-point)
    (setq-local +vterm--insert-point nil)))
