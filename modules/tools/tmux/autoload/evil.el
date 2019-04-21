;;; tools/tmux/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+tmux:run "tools/tmux/autoload/evil" nil t)
(evil-define-command +tmux:run (bang &optional command)
  (interactive "<!><fsh>")
  (if (evil-visual-state-p)
      (+tmux/send-region evil-visual-beginning evil-visual-end bang)
    (+tmux/run command bang)))

;;;###autoload (autoload '+tmux:cd-here "tools/tmux/autoload/evil" nil t)
(evil-define-command +tmux:cd-here (bang)
  (interactive "<!>")
  (if bang
      (+tmux/cd-to-here)
    (+tmux/cd-to-project)))

