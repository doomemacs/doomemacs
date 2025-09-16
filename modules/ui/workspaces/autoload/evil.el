;;; ui/workspaces/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor evil)

;;;###autoload (autoload '+workspaces:save "ui/workspaces/autoload/evil" nil t)
(evil-define-command +workspaces:save (&optional name)
  "Ex wrapper around `+workspace/save-session'."
  (interactive "<a>") (+workspaces/save name))

;;;###autoload (autoload '+workspaces:load "ui/workspaces/autoload/evil" nil t)
(evil-define-command +workspaces:load (&optional name)
  "Ex wrapper around `+workspace/load-session'."
  (interactive "<a>") (+workspaces/load name))

;;;###autoload (autoload '+workspaces:new "ui/workspaces/autoload/evil" nil t)
(evil-define-command +workspaces:new (bang name)
  "Ex wrapper around `+workspace/new'. If BANG, clone the current workspace."
  (interactive "<!><a>") (+workspaces/new name bang))

;;;###autoload (autoload '+workspaces:edit "ui/workspaces/autoload/evil" nil t)
(evil-define-command +workspaces:edit (file)
  "Open FILE in a new workspace."
  (interactive "<f>")
  (+workspaces/new)
  (find-file file))

;;;###autoload (autoload '+workspaces:rename "ui/workspaces/autoload/evil" nil t)
(evil-define-command +workspaces:rename (new-name)
  "Ex wrapper around `+workspace/rename'."
  (interactive "<a>") (+workspaces/rename new-name))

;;;###autoload (autoload '+workspaces:switch-next "ui/workspaces/autoload/evil" nil t)
(evil-define-command +workspaces:switch-next (&optional count)
  "Switch to next workspace. If COUNT, switch to COUNT-th workspace."
  (interactive "<c>")
  (if count (+workspaces/switch-to count) (tab-bar-switch-to-next-tab count)))

;;;###autoload (autoload '+workspaces:switch-previous "ui/workspaces/autoload/evil" nil t)
(evil-define-command +workspaces:switch-previous (&optional count)
  "Switch to previous workspace. If COUNT, switch to COUNT-th workspace."
  (interactive "<c>")
  (if count (+workspaces/switch-to count) (tab-bar-switch-to-prev-tab count)))
