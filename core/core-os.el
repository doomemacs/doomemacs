;;; core-os.el -*- lexical-binding: t; -*-

;; clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; fewer opts to process for systems that don't need them
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Fix the clipboard in terminal or daemon Emacs (non-GUI)
(defun doom|init-clipboard-in-tty-emacs ()
  (if IS-MAC
      (if (require 'osx-clipboard nil t) (osx-clipboard-mode))
    (if (require 'xclip nil t) (xclip-mode))))
(add-hook 'tty-setup-hook #'doom|init-clipboard-in-tty-emacs)

;; stop copying each visual state move to the clipboard:
;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
;; grokked from: http://stackoverflow.com/questions/15873346/elisp-rename-macro
(advice-add #'evil-visual-update-x-selection :override #'ignore)

(defmacro set-env! (&rest _vars)
  "Inject VARS from your shell environment into Emacs.")

(cond (IS-MAC
       (setq mac-command-modifier 'super
             mac-option-modifier  'meta
             ;; sane trackpad/mouse scroll settings
             mac-redisplay-dont-reset-vscroll t
             mac-mouse-wheel-smooth-scroll nil
             mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
             mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
             ;; Curse Lion and its sudden but inevitable fullscreen mode!
             ;; NOTE Meaningless to railwaycat's emacs-mac build
             ns-use-native-fullscreen nil
             ;; Visit files opened outside of Emacs in existing frame, rather
             ;; than a new one
             ns-pop-up-frames nil)

       (when (or (daemonp) (display-graphic-p))
         ;; Syncs ns frame parameters with theme (and fixes mismatching text
         ;; colr in the frame title)
         (when (require 'ns-auto-titlebar nil t)
           (add-hook 'doom-load-theme-hook #'ns-auto-titlebar-mode))

         ;; A known problem with GUI Emacs on MacOS (or daemons started via
         ;; launchctl or brew services): it runs in an isolated
         ;; environment, so envvars will be wrong. That includes the PATH
         ;; Emacs picks up. `exec-path-from-shell' fixes this.
         (when (require 'exec-path-from-shell nil t)
           (defun set-env! (&rest vars)
             "Inject VARS from your shell environment into Emacs."
             (exec-path-from-shell-copy-envs vars))
           (setq exec-path-from-shell-check-startup-files nil
                 exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments)
                 exec-path-from-shell-debug doom-debug-mode
                 exec-path-from-shell-variables
                 (nconc exec-path-from-shell-variables '("LC_CTYPE" "LC_ALL" "LANG")))
           (exec-path-from-shell-initialize))))

      (IS-LINUX
       (setq x-gtk-use-system-tooltips nil    ; native tooltips are ugly!
             x-underline-at-descent-line t))  ; draw underline lower

      (IS-WINDOWS
       (setq w32-get-true-file-attributes nil) ; fix file io slowdowns
       (when (display-graphic-p)
         (setenv "GIT_ASKPASS" "git-gui--askpass"))))

(provide 'core-os)
;;; core-os.el ends here
