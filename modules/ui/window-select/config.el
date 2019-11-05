;;; ui/window-select/config.el -*- lexical-binding: t; -*-

(use-package! switch-window
  :when (featurep! +switch-window)
  :defer t
  :init
  (global-set-key [remap other-window] #'switch-window)
  :config
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "d" "f" "g" "h" "j" "k" "l")))


(use-package! ace-window
  :unless (featurep! +switch-window)
  :defer t
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))


(use-package! winum
  :when (featurep! +numbers)
  :after-call doom-switch-window-hook
  :config
  (winum-mode +1)
  (map! :map evil-window-map
        "0" #'winum-select-window-0-or-10
        "1" #'winum-select-window-1
        "2" #'winum-select-window-2
        "3" #'winum-select-window-3
        "4" #'winum-select-window-4
        "5" #'winum-select-window-5
        "6" #'winum-select-window-6
        "7" #'winum-select-window-7
        "8" #'winum-select-window-8
        "9" #'winum-select-window-9))
