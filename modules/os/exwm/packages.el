;; -*- no-byte-compile: t; -*-
;;; os/exwm/packages.el

;; Here we require the `exwm' package for the window manager itself
;; and the `exwm-firefox-evil' package for firefox integration with
;; evil mode keybindings:
(package! exwm)
(if (featurep! +firefox)
    (if (featurep! :editor evil)
        package! exwm-firefox-evil)
  (package! exwm-firefox))
