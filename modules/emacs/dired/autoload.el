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
      (when-let* (((not (dirvish-curr)))
                  ((not (active-minibuffer-window)))
                  (win (dirvish-side--session-visible-p))
                  (dv (with-selected-window win (dirvish-curr)))
                  (dir (or (dirvish--get-project-root) default-directory))
                  (prev (with-selected-window win (dirvish-prop :index)))
                  (curr buffer-file-name)
                  ((not (string-suffix-p "COMMIT_EDITMSG" curr)))
                  ((not (equal prev curr))))
        (with-selected-window win
          (when dir
            (setq dirvish--this dv)
            (let (buffer-list-update-hook) (dirvish-find-entry-a dir))
            (if dirvish-side-auto-expand (dirvish-subtree-expand-to file)
              (dired-goto-file curr))
            (dirvish-prop :cus-header 'dirvish-side-header)
            (dirvish-update-body-h))))))
  (select-window (dirvish-side--session-visible-p)))
