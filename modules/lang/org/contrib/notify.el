;;; lang/org/contrib/notify.el -*- lexical-binding: t; -*-
;;;###if (featurep! +notify)

(use-package! org-wild-notifier
  :hook (org-load . org-wild-notifier-mode)
  :config
  (setq org-wild-notifier-alert-time '(60 30)))
