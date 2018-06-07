;;; ui/fci/config.el -*- lexical-binding: t; -*-

(def-package! fill-column-indicator
  :hook ((text-mode prog-mode conf-mode) . turn-on-fci-mode)
  :config
  (defun +fci|set-color ()
    (setq fci-rule-color (face-foreground 'line-number)))
  (add-hook 'doom-load-theme-hook #'+fci|set-color)
  (+fci|set-color)

  (when (featurep! :completion company)
    (add-hook 'company-completion-started-hook #'+fci|disable-when-company-activates)
    (add-hook! '(company-completion-finished-hook company-completion-cancelled-hook)
      #'+fci|enable-when-company-deactivates)))
