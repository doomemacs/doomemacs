;;; tools/wakatime/autoload.el -*- lexical-binding: t; -*-

(defvar +wakatime-home (concat doom-cache-dir "wakatime/")
  "Path to the directory where wakatime files are stored.")

(defvar +wakatime-hide-filenames nil
  "If non-nil, obfuscate files and only show what projects you're working on.")

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
    (require 'wakatime-mode)
    (customize-set-variable 'wakatime-api-key api-key)
    (customize-save-customized)
    (unless (or (and wakatime-cli-path (file-executable-p wakatime-cli-path))
                (not (equal (wakatime-find-binary "wakatime") "wakatime")))
      (user-error "Couldn't find wakatime executable (%s)"
                  (or wakatime-cli-path "wakatime")))
    (global-wakatime-mode +1)
    (message "Wakatime enabled. You're good to go!")))

;;;###autoload
(defun +wakatime|autostart (&rest _)
  "Initialize wakatime (if `wakatime-api-key' is set, otherwise no-op with a
warning)."
  (interactive)
  (require 'wakatime-mode)
  (if (not wakatime-api-key)
      (message "wakatime-mode isn't set up. Run `M-x +wakatime/setup' to do so.")
    (when +wakatime-home
      (unless (file-directory-p +wakatime-home)
        (make-directory +wakatime-home t)))
    (global-wakatime-mode +1))
  ;;
  (remove-hook 'doom-exit-buffer-hook #'+wakatime|autostart)
  (advice-remove 'after-find-file #'+wakatime|autostart))

;;;###autoload
(defun +wakatime|delayed-autostart (&rest _)
  "Lazily initialize `wakatime-mode' until the next time you switch buffers or
open a file."
  (add-hook 'doom-exit-buffer-hook #'+wakatime|autostart)
  ;; this is necessary in case the user opens emacs with file arguments
  (advice-add 'after-find-file :before #'+wakatime|autostart))

(defun +wakatime*append-options (ret)
  "Modifies the wakatime command string so that `+wakatime-hide-filenames' and
`+wakatime-home' are respected."
  (concat (when +wakatime-home
            (format "WAKATIME_HOME=%s " (shell-quote-argument +wakatime-home)))
          ret
          (if +wakatime-hide-filenames " --hide-filenames")))
(advice-add #'wakatime-client-command :filter-return #'+wakatime*append-options)
