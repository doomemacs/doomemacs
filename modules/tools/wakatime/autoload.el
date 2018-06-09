;;; tools/wakatime/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-hook 'doom-after-switch-buffer-hook #'+wakatime-init)

;;;###autoload
(defvar wakatime-api-key nil)

;;;###autoload
(defun +wakatime-init ()
  "Initialize wakatime (if `wakatime-api-key' is set, otherwise no-op with a
warning)."
  (if wakatime-api-key
      (global-wakatime-mode +1)
    (message "No `wakatime-api-key' set! wakaktime-mode will stay disabled."))
  (remove-hook 'doom-after-switch-buffer-hook #'+wakatime-init))
