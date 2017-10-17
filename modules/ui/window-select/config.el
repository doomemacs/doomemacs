;;; ui/window-select/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +switch-window)
       ;; Configure switch-window if that flag is found
       (def-package! switch-window
         :commands (switch-window switch-window-then-maximize switch-window-then-split-below
                    switch-window-then-split-right switch-window-then-delete
                    switch-window-then-swap-buffer)
         :init
         (setq switch-window-shortcut-style 'qwerty
               switch-window-qwerty-shortcuts '("a" "s" "d" "f" "g" "h" "j" "k" "l"))
         ;; Redefine how we switch windows with switch-window
         (define-key global-map [remap other-window] #'switch-window)))
      ((or (featurep! +ace-window) t)
       ;; Configure ace-window if that flag or no flag is found
       (def-package! ace-window
         :commands (ace-window ace-swap-window ace-delete-window
                               ace-select-window ace-delete-other-windows)
         :init
         (define-key global-map [remap other-window] #'ace-window)
         :config
         (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
               aw-scope 'frame
               aw-background t))))
