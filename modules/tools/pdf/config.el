;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(def-package! pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :init (load "pdf-tools-autoloads" nil t)
  :config
  (unless noninteractive
    (pdf-tools-install))

  (map! :map pdf-view-mode-map "q" #'kill-this-buffer)

  (setq-default pdf-view-display-size 'fit-page)
  ;; turn off cua so copy works
  (add-hook! 'pdf-view-mode-hook (cua-mode 0)))
