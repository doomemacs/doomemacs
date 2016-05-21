;;; extra-tmux.el

;; This library offers:
;;   + A way of communicating with a tmux instance
;;   + TODO A way to manage tmuxifier from emacs

;;;###autoload
(defun tmux (command &optional modes)
  (let ((format
         (concat "tmux send-keys "
                 (if (or (eq modes t)
                         (eq modes 'clear)
                         (memq 'clear modes))
                     "C-u " "")
                 "%s"
                 (if (or (eq modes t)
                         (eq modes 'run)
                         (memq 'run modes))
                     " Enter" ""))))
    (shell-command (format format (shell-quote-argument command)))))

(evil-define-interactive-code "<term>"
  "Ex tmux argument (a mix between <sh> <f> and <fsh>)"
  :ex-arg shell
  (list (when (evil-ex-p) (evil-ex-file-arg))))

;;;###autoload (autoload 'doom:tmux-cd "extra-tmux" nil t)
(evil-define-command doom:tmux-cd (&optional bang)
  (interactive "<!>")
  (if bang
      (doom/tmux-cd-to-project)
    (doom/tmux-cd-to-here)))

(defvar doom-tmux-last-command nil "The last command sent to tmux")
;;;###autoload (autoload 'doom:tmux "extra-tmux" nil t)
(evil-define-operator doom:tmux (&optional command bang)
  "Sends input to tmux. Use `bang' to append to tmux"
  :type inclusive
  (interactive "<term><!>")
  (unless command
    (setq command doom-tmux-last-command))
  (if (not command)
      (os-switch-to-term)
    (tmux command (not bang))
    (setq doom-tmux-last-command command)
    (when (evil-ex-p)
      (message "[Tmux] %s" command))))

;;;###autoload
(defun doom/tmux-cd-to-here (&optional dir)
  (interactive)
  (tmux (format "cd '%s'" (or dir default-directory))))

;;;###autoload
(defun doom/tmux-cd-to-project ()
  (interactive)
  (doom/tmux-cd-to-here (doom/project-root)))

(provide 'extra-tmux)
;;; extra-tmux.el ends here
