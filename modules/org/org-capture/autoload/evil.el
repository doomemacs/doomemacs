;;; org/org-capture/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+org-capture:open "org/org-capture/autoload/evil" nil t)
(evil-define-operator +org-capture:open (&optional beg end)
  "Evil ex interface to `+org-capture/dwim'."
  :move-point nil :type inclusive
  (interactive "<r>")
  (+org-capture/open
   (unless (or (evil-normal-state-p) (evil-insert-state-p))
     (buffer-substring beg end))))
