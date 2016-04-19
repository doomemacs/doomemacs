;;; extra-tmux.el

;; This library offers:
;;   + TODO An integration/abstraction layer to make it seem like tmux and emacs are one
;;     program.
;;   + TODO A way to manage tmux sessions and layouts from emacs; possibly trigger them
;;     depending on current project.

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

;;;###autoload (autoload 'narf:tmux-cd "extra-tmux" nil t)
(evil-define-command narf:tmux-cd (&optional bang)
  (interactive "<!>")
  (if bang
      (narf/tmux-cd-to-project)
    (narf/tmux-cd-to-here)))

(defvar narf-tmux-last-command nil "The last command sent to tmux")
;;;###autoload (autoload 'narf:tmux "extra-tmux" nil t)
(evil-define-operator narf:tmux (&optional command bang)
  "Sends input to tmux. Use `bang' to append to tmux"
  :type inclusive
  (interactive "<term><!>")
  (unless command
    (setq command narf-tmux-last-command))
  (if (not command)
      (os-switch-to-term)
    (tmux command (not bang))
    (setq narf-tmux-last-command command)
    (when (evil-ex-p)
      (message "[Tmux] %s" command))))

;;;###autoload
(defun narf/tmux-cd-to-here (&optional dir)
  (interactive)
  (tmux (format "cd '%s'" (or dir default-directory))))

;;;###autoload
(defun narf/tmux-cd-to-project ()
  (interactive)
  (narf/tmux-cd-to-here (narf/project-root)))

(provide 'extra-tmux)
;;; extra-tmux.el ends here
