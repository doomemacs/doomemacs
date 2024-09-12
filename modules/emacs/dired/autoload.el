;;; emacs/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dired/quit-all ()
  "Kill all `dired-mode' buffers."
  (interactive)
  (mapc #'kill-buffer (doom-buffers-in-mode 'dired-mode))
  (message "Killed all dired buffers"))

;;;###autoload
(defun +dired-enable-git-info-h ()
  "Enable `dired-git-info-mode' in git repos."
  (and (not (file-remote-p default-directory))
       (locate-dominating-file "." ".git")
       (dired-git-info-mode 1)))

;;;###autoload
(defun +dired/dirvish-side-and-follow (&optional arg)
  "Open `dirvish-side' then find the currently focused file.

If dirvish is already open, remotely jump to the file in Dirvish.
If given the prefix ARG, then prompt for a directory (replaces existing Dirvish
sidebars)."
  (interactive "P")
  (require 'dirvish-side)
  (save-selected-window
    (let ((win (dirvish-side--session-visible-p)))
      (when (and win arg)
        (with-selected-window win
          (dirvish-quit))
        (setq win nil))
      (unless win
        (call-interactively #'dirvish-side))
      (when-let* (((not dirvish--this))
                  (dir (or (dirvish--get-project-root) default-directory))
                  (win (dirvish-side--session-visible-p))
                  (dv (with-selected-window win (dirvish-curr)))
                  ((not (active-minibuffer-window)))
                  (file buffer-file-name))
        (with-selected-window win
          (when dir
            (setq dirvish--this dv)
            (let (buffer-list-update-hook) (dirvish-find-entry-a dir))
            (if dirvish-side-auto-expand (dirvish-subtree-expand-to file)
              (dired-goto-file file))
            (dirvish-prop :cus-header 'dirvish-side-header)
            (dirvish--setup-mode-line (car (dv-layout dv)))
            (dirvish-update-body-h))
          (setq dirvish--this nil)))))
  (select-window (dirvish-side--session-visible-p)))
