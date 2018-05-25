;;; ui/window-select/config.el -*- lexical-binding: t; -*-

(def-package! switch-window
  :when (featurep! +switch-window)
  :defer t
  :init
  (define-key global-map [remap other-window] #'switch-window)
  :config
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "d" "f" "g" "h" "j" "k" "l")))


(def-package! ace-window
  :unless (featurep! +switch-window)
  :defer t
  :init
  (define-key global-map [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))
