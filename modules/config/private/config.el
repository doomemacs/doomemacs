;;; config/private/config.el -*- lexical-binding: t; -*-

(defun +private|load-config ()
  "Loads your private config.el in `+private-config-path'."
  (load (expand-file-name "config.el" +private-config-path)
        'noerror 'nomessage))

(add-hook 'after-init-hook #'+private|load-config)
