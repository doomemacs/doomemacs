;;; feature/workspaces/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+workspace:save-session "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:save-session (&optional bang name)
  "Ex wrapper around `+workspace/save-session'. If BANG, then autosave
(pointless if autosaving/loading is off). If NAME is nil, default to 'last'."
  (interactive "<!><a>")
  (+workspace/save-session (if bang persp-auto-save-fname name)))

;;;###autoload (autoload '+workspace:load-session "feature/workspaces/autoload/evil" nil t)
(evil-define-command +workspace:load-session (&optional bang name)
  "Ex wrapper around `+workspace/load-session'. If BANG, then load last autosave
(pointless if autosaving/loading is off). If NAME is nil, defaults to 'last'."
  (interactive "<!><a>")
  (+workspace/load-session (if bang persp-auto-save-fname name)))

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
  (interactive) (+workspace/delete (+workspace-current-name)))

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
