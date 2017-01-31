
(setq mac-command-modifier 'meta
      mac-option-modifier  'alt
      ;; sane trackpad/mouse scroll settings
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      ;; Curse Lion and its sudden but inevitable fullscreen mode!
      ;; NOTE Meaningless to railwaycat's emacs-mac build
      ns-use-native-fullscreen nil
      ;; Don't open files from the workspace in a new frame
      ns-pop-up-frames nil)

(cond ((display-graphic-p)
       ;; `exec-path-from-shell' is slow, so bring out the cache
       (setq exec-path
             (or (persistent-soft-fetch 'exec-path "emacs")
                 (persistent-soft-store
                  'exec-path
                  (progn (require 'exec-path-from-shell)
                         (exec-path-from-shell-initialize)
                         exec-path)
                  "emacs"))))
      (t
       (require 'osx-clipboard)
       (osx-clipboard-mode +1)))

(after! evil
  ;; On OSX, stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; Most of this code grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (when (or (featurep 'mac) (featurep 'ns))
    (advice-add 'evil-visual-update-x-selection :override 'ignore)))
