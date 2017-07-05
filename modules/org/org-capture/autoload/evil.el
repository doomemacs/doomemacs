;;; org/org-capture/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+org-capture:dwim "org/org-capture/autoload/evil" nil t)
(evil-define-operator +org-capture:dwim (&optional beg end)
  "Evil ex interface to `+org-capture/dwim'."
  :move-point nil :type inclusive
  (interactive "<r>")
  (+org-capture/dwim
   (unless (or (evil-normal-state-p) (evil-insert-state-p))
     (buffer-substring beg end))))
