;;; emacs/eshell/autoload/commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun eshell/cd-to-project ()
  "Change to the project root of the current directory."
  (let* ((default-directory (eshell/pwd))
         (project-root (doom-project-root 'nocache)))
    (eshell/cd project-root)))

;;;###autoload
(defun eshell/quit-and-close (&rest _)
  "Quit the current eshell buffer and close the window it's in."
  (setq-local +eshell-kill-window-on-exit t)
  (throw 'eshell-terminal t))

;;;###autoload
(defun eshell/mkdir-and-cd (dir)
  "Create a directory then cd into it."
  (make-directory dir t)
  (eshell/cd dir))
