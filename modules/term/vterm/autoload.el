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
      (require 'vterm)
      (setenv "PROOT" (or (doom-project-root) default-directory))
      (let ((buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (doom-mark-buffer-as-real-h)
          (unless (eq major-mode 'vterm-mode)
            (vterm-mode)))
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
  (let ((default-directory
          (if arg
              default-directory
            (or (doom-project-root) default-directory))))
    (vterm)))
