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
    :init (exec-path-from-shell-initialize)))
