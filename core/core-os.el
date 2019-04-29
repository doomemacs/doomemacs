;;; core-os.el -*- lexical-binding: t; -*-

;; TODO Remove me later (deprecated)
(defmacro set-env! (&rest _))

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

;; Enable mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; stop copying each visual state move to the clipboard:
;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
;; grokked from: http://stackoverflow.com/questions/15873346/elisp-rename-macro
(advice-add #'evil-visual-update-x-selection :override #'ignore)

(cond (IS-MAC
       (setq mac-command-modifier 'super
             mac-option-modifier  'meta
             ;; sane trackpad/mouse scroll settings
             mac-redisplay-dont-reset-vscroll t
             mac-mouse-wheel-smooth-scroll nil
             ;; Curse Lion and its sudden but inevitable fullscreen mode!
             ;; NOTE Meaningless to railwaycat's emacs-mac build
             ns-use-native-fullscreen nil
             ;; Visit files opened outside of Emacs in existing frame, rather
             ;; than a new one
             ns-pop-up-frames nil)

       ;; Syncs ns frame parameters with theme (and fixes mismatching text color
       ;; in the frame title)
       (when (and (or (daemonp)
                      (display-graphic-p))
                  (require 'ns-auto-titlebar nil t))
         (add-hook 'doom-load-theme-hook #'ns-auto-titlebar-mode)))

      (IS-LINUX
       (setq x-gtk-use-system-tooltips nil    ; native tooltips are ugly!
             x-underline-at-descent-line t))  ; draw underline lower

      (IS-WINDOWS
       (setq w32-get-true-file-attributes nil) ; fix file io slowdowns
       (when (display-graphic-p)
         (setenv "GIT_ASKPASS" "git-gui--askpass"))))

(provide 'core-os)
;;; core-os.el ends here
