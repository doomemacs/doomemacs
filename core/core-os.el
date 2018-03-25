;;; core-os.el -*- lexical-binding: t; -*-

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; Use a shared clipboard
      select-enable-clipboard t
      select-enable-primary t)

;; stop copying each visual state move to the clipboard:
;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
;; Most of this code grokked from:
;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
(advice-add #'evil-visual-update-x-selection :override #'ignore)

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
              ;; A known problem with GUI Emacs on MacOS: it runs in an isolated
              ;; environment, so envvars will be wrong. That includes the PATH
              ;; Emacs picks up. `exec-path-from-shell' fixes this.
              (when (require 'exec-path-from-shell nil t)
                (def-setting! :env (&rest vars)
                  "Inject VARS from your shell environment into Emacs."
                  `(exec-path-from-shell-copy-envs (list ,@vars)))
                (setq exec-path-from-shell-check-startup-files nil
                      exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
                (defvaralias 'exec-path-from-shell-debug 'doom-debug-mode)
                (exec-path-from-shell-initialize)))
             ((require 'osx-clipboard nil t)
              (osx-clipboard-mode +1))))

      (IS-LINUX
       (setq x-gtk-use-system-tooltips nil    ; native tooltips are ugly!
             x-underline-at-descent-line t))  ; draw underline lower

      (IS-WINDOWS
       (setq w32-get-true-file-attributes nil) ; fix file io slowdowns
       ))

(provide 'core-os)
;;; core-os.el ends here
