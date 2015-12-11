;;; defuns-tmux.el

;;;###autoload
(defun tmux (command &optional run)
  (shell-command (format (concat "tmux send-keys " (if run "C-u %s Enter" "%s"))
                         (shell-quote-argument command))))

(evil-define-interactive-code "<term>"
  "Ex tmux argument (a mix between <sh> <f> and <fsh>)"
  :ex-arg shell
  (list (when (evil-ex-p) (evil-ex-file-arg))))

;;;###autoload (autoload 'narf:tmux-cd "defuns-term" nil t)
(evil-define-command narf:tmux-cd (&optional bang)
  (interactive "<!>")
  (if bang
      (narf/tmux-cd-to-project)
    (narf/tmux-cd-to-here)))

;;;###autoload (autoload 'narf:tmux "defuns-term" nil t)
(evil-define-operator narf:tmux (beg end &optional command bang)
  "Sends input to tmux. Use `bang' to append to tmux"
  :type inclusive
  :repeat t
  (interactive "<r><term><!>")
  (if (not command)
      (os-switch-to-term)
    (when (use-region-p)
      (setq command (concat command (buffer-substring-no-properties beg end))))
    (tmux command bang)
    (when (evil-ex-p)
      (message "[Tmux] %s" command))))

;;;###autoload
(defun narf/tmux-new-window ()
  (interactive)
  (tmux "tmux new-window" t))

;;;###autoload
(defun narf/tmux-split-window (&optional vertical)
  (interactive)
  (tmux (concat "tmux split-window" (if vertical " -h"))))

;;;###autoload
(defun narf/tmux-vsplit-window ()
  (interactive)
  (narf/tmux-split-window t))

;;;###autoload
(defun narf/tmux-cd-to-here (&optional dir)
  (interactive)
  (tmux (format "cd '%s'" (or dir default-directory))))

;;;###autoload
(defun narf/tmux-cd-to-project ()
  (interactive)
  (narf/tmux-cd-to-here (narf/project-root)))

(provide 'defuns-tmux)
;;; defuns-tmux.el ends here
