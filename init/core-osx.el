
;; Use a shared clipboard
(setq x-select-enable-clipboard t)
;; Curse you Lion-esque fullscreen mode!
(setq ns-use-native-fullscreen nil)
;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

(my/install-package 'exec-path-from-shell)    ; fix emacs PATH on OSX
(use-package exec-path-from-shell
  :if window-system
  :init (exec-path-from-shell-initialize))

;;
(provide 'core-osx)
