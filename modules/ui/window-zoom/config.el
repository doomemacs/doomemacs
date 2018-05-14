;;; ui/window-zoom/config.el -*- lexical-binding: t; -*-

;; FIXME zoom breaks with doom's popup system
(def-package! zoom
  :when (featurep! +zoom)
  :config
  (setq zoom-ignored-buffer-name-regexps '("\*")
        zoom-size '(0.618 . 0.618)
        zoom-ignored-major-modes '(+popup-mode))
  (zoom-mode 1))

(def-package! golden-ratio
  :unless (featurep! +zoom)
  :config
  (setq golden-ratio-auto-scale t
        golden-ratio-exclude-buffer-regexp '("\*")
        ;; golden ratio needs to know evil windows exists
        golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  buf-move-left
                  buf-move-right
                  buf-move-up
                  buf-move-down
                  window-number-select
                  select-window
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  select-window-6
                  select-window-7
                  select-window-8
                  select-window-9)))
  (golden-ratio-mode 1))
