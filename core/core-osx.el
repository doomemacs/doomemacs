;; Mac-specific settings

;; Use a shared clipboard
(setq x-select-enable-clipboard t)
;; Curse Lion and its sudden but inevitable fullscreen mode!
(setq ns-use-native-fullscreen nil)
;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; fix emacs PATH on OSX (GUI only)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init (exec-path-from-shell-initialize))

(after "evil"
  ;; On OSX, stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
    (unless (featurep 'ns) ad-do-it)))

;; Send current file to OSX apps
(defun my--open-file-with (path &optional appName)
  (if (and appName
           (stringp appName)
           (not (string= "" appName)))
      (setq appName (concat "-a " appName ".app")))
  (shell-command (concat "open " appName " " (shell-quote-argument path))))

(defun my-open-with (appName file)
  (interactive "sApp name: ")
  (my--open-file-with file appName))

(defun my-send-to-transmit (file)
  (interactive "f")
  (my-open-with "Transmit" file))

(defun my-send-to-launchbar (file)
  (interactive "f")
  (my-open-with "LaunchBar" file))

(defun my-send-dir-to-launchbar (dir)
  (interactive "D")
  (my--open-file-with dir "LaunchBar"))

(defun my-send-dir-to-finder (dir)
  (interactive "D")
  (my--open-file-with dir "Finder"))


(provide 'core-osx)
