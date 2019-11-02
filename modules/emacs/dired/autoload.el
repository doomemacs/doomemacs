;;; emacs/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dired/quit-all ()
  "Kill all `dired-mode' buffers."
  (interactive)
  (mapc #'kill-buffer (doom-buffers-in-mode 'dired-mode))
  (message "Killed all dired buffers"))


;;;###autoload
(defun +dired-enable-git-info-h ()
  (if (and
       (not (file-remote-p default-directory))
       (locate-dominating-file "." ".git"))
      (dired-git-info-mode 1)))

;;;###autoload
(defun +dired-dotfiles-hide ()
  (set (make-local-variable '+dired-dotfiles-show-p) nil)
  (dired-mark-files-regexp "^\\\.")
  (dired-do-kill-lines))

;;;###autoload
(defun +dired/dotfiles-toggle ()
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp '+dired-dotfiles-show-p)) +dired-dotfiles-show-p) ; if currently showing
        (+dired-dotfiles-hide)
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable '+dired-dotfiles-show-p) t)))))
