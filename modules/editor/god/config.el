;;; editor/god/config.el -*- lexical-binding: t; -*-

(use-package! god-mode
  :hook (doom-after-init-modules . god-mode-all)
  :config
  (pushnew! god-exempt-major-modes
            'Custom-mode
            'Info-mode
            'ag-mode
            'calculator-mode
            'calendar-mode
            'cider-test-report-mode
            'compilation-mode
            'debugger-mode
            'edebug-mode
            'ediff-mode
            'eww-mode
            'geben-breakpoint-list-mode
            'ibuffer-mode
            'org-agenda-mode
            'pdf-outline-buffer-mode
            'recentf-dialog-mode
            'sldb-mode
            'sly-db-mode
            'wdired-mode)

  (add-hook 'post-command-hook #'+god--configure-cursor-and-modeline-h)
  (add-hook 'overwrite-mode-hook #'+god--toggle-on-overwrite-h))
