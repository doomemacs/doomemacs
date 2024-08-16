;;; os/macos/config.el -*- lexical-binding: t; -*-

;;
;;; Reasonable defaults for macOS

;; Use spotlight search backend as a default for M-x locate (and helm/ivy
;; variants thereof), since it requires no additional setup.
(setq locate-command "mdfind")


;;
;;; Compatibilty fixes

;; Curse Lion and its sudden but inevitable fullscreen mode!
;; This is meaningless to railwaycat's emacs-mac build though.
(setq ns-use-native-fullscreen nil)

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; Sane trackpad/mouse scroll settings. Also disables smooth scrolling because
;; it's disturbingly clunky and slow without something like
;; jdtsmith/ultra-scroll-mac.
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so window
;; borders will match the enabled theme.
(and (or (daemonp)
         (display-graphic-p))
     (require 'ns-auto-titlebar nil t)
     (ns-auto-titlebar-mode +1))

;; Integrate with Keychain
(after! auth-source
  (pushnew! auth-sources 'macos-keychain-internet 'macos-keychain-generic))

;; Delete files to trash on macOS, as an extra layer of precaution against
;; accidentally deleting wanted files.
(setq delete-by-moving-to-trash (not noninteractive))


;;
;;; Packages

(use-package! osx-trash
  ;; DEPRECATED: Not needed on Emacs 29+. Remove when dropping 28 support.
  ;;   Fixed by https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21340.
  :when (< emacs-major-version 29)
  :commands osx-trash-move-file-to-trash
  :init
  ;; Lazy load `osx-trash'
  (when (not (fboundp 'system-move-file-to-trash))
    (defun system-move-file-to-trash (file)
      "Move FILE to trash."
      (when (and (not (featurep :system 'linux))
                 (not (file-remote-p default-directory)))
        (osx-trash-move-file-to-trash file)))))
