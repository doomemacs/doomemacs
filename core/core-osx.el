;; Mac-specific settings

;; Use a shared clipboard
(setq x-select-enable-clipboard t)
;; Curse Lion and its sudden but inevitable fullscreen mode!
(setq ns-use-native-fullscreen nil)
;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Prefixes: Command = M, Alt = A
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)

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
(defun my-open-with (&optional app-name path)
  (interactive)
  (let ((app-name (if app-name (concat "-a " app-name)))
        (path (or path (if (eq major-mode 'dired-mode) (dired-get-file-for-visit) (buffer-file-name)))))
    (message "Trying: %s" (concat "open " app-name " " (shell-quote-argument path)))
    (shell-command (concat "open " app-name " " (shell-quote-argument path)))))


(provide 'core-osx)
;;; core-osx.el ends here
