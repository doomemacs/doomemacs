;;; term/eshell/autoload/commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun eshell/emacs (&rest files)
  "Open a FILES in Emacs.
For folks with a habit of using \"emacs\" to open files, even in eshell."
  (if files
      (mapc #'find-file
            (mapcar #'expand-file-name
                    (eshell-flatten-list (reverse files))))
    (bury-buffer)))
;;;###autoload
(defalias 'eshell/e #'eshell/emacs)

;;;###autoload
(defun eshell/cd-to-project ()
  "Change to the project root of the current directory."
  (eshell/cd (doom-project-root (eshell/pwd))))

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

;;;###autoload
(defalias 'eshell/help #'+eshell-lookup-documentation)
