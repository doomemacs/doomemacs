;; -*- no-byte-compile: t; -*-
;;; os/exwm/packages.el

;; Here we require the `exwm' package for the window manager itself
;; and the `exwm-firefox-evil' package for firefox integration with
;; evil mode keybindings:
;; NOTE This section is for testing:
(package! exwm)
(package! exwm-firefox-evil)

;; NOTE This section is for when the module is function within the
;; =doom/modules= directory:
;; (if (featurep! :firefox)
;;     (if (featurep! :editor evil)
;;         (package! exwm-firefox-evil)
;;       (package! exwm-firefox)))
