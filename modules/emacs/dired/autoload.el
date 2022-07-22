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
(defun +dired-subtree-remove-all ()
  "Remove all subtrees in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eq (point) (progn (re-search-forward dired-re-dir nil t 1) (point))))
      (when (dired-subtree--is-expanded-p)
        (forward-line 1)
        (dired-subtree-remove)))))
