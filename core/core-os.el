;;; core-os.el

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

 ;; clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; Use a shared clipboard
      select-enable-clipboard t
      select-enable-primary t)


(cond (IS-MAC
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
                        (and (require 'exec-path-from-shell nil t)
                             (progn (exec-path-from-shell-initialize)
                                    (persistent-soft-store 'exec-path exec-path "emacs")))
                        exec-path)))
             (t
              (when (require 'osx-clipboard nil t)
                (osx-clipboard-mode +1))))

       (after! evil
         ;; On OSX, stop copying each visual state move to the clipboard:
         ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
         ;; Most of this code grokked from:
         ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
         (when (or (featurep 'mac) (featurep 'ns))
           (advice-add 'evil-visual-update-x-selection :override 'ignore))))

      (IS-LINUX
       ;; nothing yet
       ))

(provide 'core-os)
;;; core-os.el ends here
