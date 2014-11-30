(provide 'core-osx)

(when is-mac
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

  ;; On OSX, stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (defadvice evil-visual-update-x-selection (around clobber-x-select-text activate)
    (unless (featurep 'ns) ad-do-it)))
