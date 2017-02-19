;;; feature/workspaces/autoload/evil.el

;;;###autoload (autoload '+workspace:save-session "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:save-session (&optional name)
  "Ex wrapper around `+workspace/save-session'."
  (interactive "<a>") (+workspace/save-session name))

;;;###autoload (autoload '+workspace:load-session "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:load-session (&optional name)
  "Ex wrapper around `+workspace/load-session'."
  (interactive "<a>") (+workspace/load-session name))

;;;###autoload (autoload '+workspace:save "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:save (&optional name)
  "Ex wrapper around `+workspace/save-session'."
  (interactive "<a>") (+workspace/save name))

;;;###autoload (autoload '+workspace:load "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:load (&optional name)
  "Ex wrapper around `+workspace/load-session'."
  (interactive "<a>") (+workspace/load name))

;;;###autoload (autoload '+workspace:new "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:new (bang name)
  "Ex wrapper around `+workspace/new'. If BANG, clone the current workspace."
  (interactive "<!><a>") (+workspace/new name bang))

;;;###autoload (autoload '+workspace:rename "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:rename (new-name)
  "Ex wrapper around `+workspace/rename'."
  (interactive "<a>") (+workspace/rename new-name))

;;;###autoload (autoload '+workspace:delete "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:delete ()
  "Ex wrapper around `+workspace/delete'."
  (interactive "<a>") (+workspace/delete (+workspace-current-name)))

;;;###autoload (autoload '+workspace:switch-next "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:switch-next (&optional count)
  "Switch to next workspace. If COUNT, switch to COUNT-th workspace."
  (interactive "<c>")
  (if count (+workspace/switch-to count) (+workspace/cycle +1)))

;;;###autoload (autoload '+workspace:switch-previous "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:switch-previous (&optional count)
  "Switch to previous workspace. If COUNT, switch to COUNT-th workspace."
  (interactive "<c>")
  (if count (+workspace/switch-to count) (+workspace/cycle -1)))
