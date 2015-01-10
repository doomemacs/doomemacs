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

(defun my-open-with (appName)
  (interactive "sApp name: ")
  (my--open-file-with buffer-file-name appName))

(defun my-send-to-transmit ()
  (interactive) (my-open-with "Transmit"))

(defun my-send-to-launchbar ()
  (interactive) (my-open-with "LaunchBar"))

(defun my-send-dir-to-launchbar ()
  (interactive) (my--open-file-with default-directory "LaunchBar"))

(defun my-send-dir-to-finder ()
  (interactive) (my--open-file-with default-directory "Finder"))


(provide 'core-osx)
