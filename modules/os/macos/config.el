;;; os/macos/config.el -*- lexical-binding: t; -*-

;;
;;; Reasonable defaults for macOS

;; Use spotlight search backend as a default for M-x locate (and helm/ivy
;; variants thereof), since it requires no additional setup.
(setq locate-command "mdfind")


;;
;;; Compatibilty fixes

;; Curse Lion and its sudden but inevitable fullscreen mode!
;; NOTE Meaningless to railwaycat's emacs-mac build
(setq ns-use-native-fullscreen nil)

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; sane trackpad/mouse scroll settings
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so window
;; borders will match the enabled theme.
(and (or (daemonp)
         (display-graphic-p))
     (require 'ns-auto-titlebar nil t)
     (ns-auto-titlebar-mode +1))

;; HACK On MacOS, disabling the menu bar makes MacOS treat Emacs as a
;;      non-application window -- which means it doesn't automatically capture
;;      focus when it is started, among other things, so enable the menu-bar for
;;      GUI frames, but keep it disabled in terminal frames because there it
;;      activates an ugly, in-frame menu bar.
(add-hook! '(window-setup-hook after-make-frame-functions)
  (defun doom-init-menu-bar-in-gui-frames-h (&optional frame)
    "Re-enable menu-bar-lines in GUI frames."
    (when-let (frame (or frame (selected-frame)))
      (when (display-graphic-p frame)
        (set-frame-parameter frame 'menu-bar-lines 1)))))

;; Integrate with Keychain
(after! auth-source
  (pushnew! auth-sources 'macos-keychain-internet 'macos-keychain-generic))


;;
;;; Packages

(use-package! osx-trash
  :commands osx-trash-move-file-to-trash
  :init
  ;; Delete files to trash on macOS, as an extra layer of precaution against
  ;; accidentally deleting wanted files.
  (setq delete-by-moving-to-trash t)

  ;; Lazy load `osx-trash'
  (and IS-MAC
       (not (fboundp 'system-move-file-to-trash))
       (defalias #'system-move-file-to-trash #'osx-trash-move-file-to-trash)))
