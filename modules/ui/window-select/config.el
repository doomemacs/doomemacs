;;; ui/window-select/config.el -*- lexical-binding: t; -*-

(def-package! switch-window
  :when (featurep! +switch-window)
  :commands (switch-window switch-window-then-maximize switch-window-then-split-below
                           switch-window-then-split-right switch-window-then-delete
                           switch-window-then-swap-buffer)
  :init
  (define-key global-map [remap other-window] #'switch-window)
  :config
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "d" "f" "g" "h" "j" "k" "l")))


(def-package! ace-window
  :unless (featurep! +switch-window)
  :commands (ace-window ace-swap-window ace-delete-window
                        ace-select-window ace-delete-other-windows)
  :init
  (define-key global-map [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))
