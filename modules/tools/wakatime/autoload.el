;;; tools/wakatime/autoload.el -*- lexical-binding: t; -*-

(defvar +wakatime-api-file (concat doom-cache-dir "wakatime.el")
  "Where the wakatime api key is cached.")

;;;###autoload
(add-hook 'doom-post-init-hook #'+wakatime|delayed-autostart)

;;;###autoload
(defun +wakatime/setup ()
  "Setup Wakatime in Emacs and start `global-wakatime-mode'.

This will prompt you for your api key. You only need to run this when your api
changes."
  (interactive)
  (when (y-or-n-p "No API key is registered. Open a browser on the wakatime api key page?")
    (browse-url "https://wakatime.com/settings/api-key"))
  (let ((api-key (read-string "Enter your wakatime API key: ")))
    (unless api-key
      (user-error "No api key was received."))
    (setq wakatime-api-key api-key)
    (with-temp-file +wakatime-api-file
      (prin1 `(setq wakatime-api-key ,wakatime-api-key)
             (current-buffer)))
    (require 'wakatime-mode)
    (global-wakatime-mode +1)
    (message "Wakatime enabled. You're good to go!")))

;;;###autoload
(defun +wakatime|autostart (&rest _)
  "Initialize wakatime (if `wakatime-api-key' is set, otherwise no-op with a
warning)."
  (interactive)
  (unless (bound-and-true-p wakatime-api-key)
    (ignore-errors (load +wakatime-api-file t t)))
  (if (bound-and-true-p wakatime-api-key)
      (global-wakatime-mode +1)
    (message "wakatime-mode isn't set up. Run `M-x +wakatime/start' to do so."))
  ;;
  (remove-hook 'doom-before-switch-buffer-hook #'+wakatime|autostart)
  (advice-remove 'after-find-file #'+wakatime|autostart))

;;;###autoload
(defun +wakatime|delayed-autostart (&rest _)
  "Lazily initialize `wakatime-mode' until the next time you switch buffers or
open a file."
  (add-hook 'doom-before-switch-buffer-hook #'+wakatime|autostart)
  ;; this is necessary in case the user opens emacs with file arguments
  (advice-add 'after-find-file :before #'+wakatime|autostart))
