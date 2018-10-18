;;; app/pacfiles/config.el -*- lexical-binding: t; -*-

(def-package! pacfiles-mode
  :defer t
  :config
  ;; pacfiles manages its own popups, tell DOOM to ignore pacfiles popups
  (set-popup-rule! "\\*pacfiles:.*" :ignore t)
  ;; setup evil bindings
  (if (featurep! :feature evil +everywhere)
      (define-key! pacfiles-mode-map
        "J" #'forward-button
        "K" #'backward-button
        "g" nil)))
