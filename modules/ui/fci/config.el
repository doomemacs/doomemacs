;;; ui/fci/config.el -*- lexical-binding: t; -*-

(defvar +fci-rule-color-function
  (if EMACS26+
      (lambda () (face-foreground 'line-number))
    (lambda () (face-foreground 'font-lock-comment-face)))
  "Color used to draw the fill-column rule.

Accepts a color string or a function that returns a color.

Changes to this variable do not take effect until `fci-mode' is restarted.")


;;
;; Packages

(def-package! fill-column-indicator
  :hook ((text-mode prog-mode conf-mode) . turn-on-fci-mode)
  :config
  ;; fci is broken in `org-mode' when `org-indent-mode' is active. org-indent is
  ;; more important to me, so...
  (add-hook 'org-mode-hook #'turn-off-fci-mode)

  (defun +fci|set-color ()
    "Automatically change `fci-rule-color' based on `+fci-rule-color-function's
return value. To disable this, either set `+fci-rule-color-function' to nil or
remove `+fci|set-color' from `fci-mode-hook'."
    (when (functionp +fci-rule-color-function)
      (setq fci-rule-color (funcall +fci-rule-color-function))))
  (add-hook 'fci-mode-hook #'+fci|set-color)

  (when (featurep! :completion company)
    (add-hook 'company-completion-started-hook #'+fci|disable-when-company-activates)
    (add-hook! '(company-completion-finished-hook company-completion-cancelled-hook)
      #'+fci|enable-when-company-deactivates)))
