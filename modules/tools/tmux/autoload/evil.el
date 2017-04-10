;;; tools/tmux/autoload/evil.el

;;;###autoload (autoload '+tmux:run "tools/tmux/autoload/evil" nil t)
(evil-define-command +tmux:run (command bang)
  (interactive "<fsh><!>")
  (+tmux/run command bang))

;;;###autoload (autoload '+tmux:cd-here "tools/tmux/autoload/evil" nil t)
(evil-define-command +tmux:cd-here (bang)
  (interactive "<!>")
  (if bang
      (+tmux/cd-to-here)
    (+tmux/cd-to-project)))

