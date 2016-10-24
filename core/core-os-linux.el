;;; core-os-linux.el --- Debian-specific settings

(setq x-super-keysym 'meta
      x-meta-keysym  'alt)

(defun doom-open-with (command &rest args)
  "Open PATH in APP-NAME, using xdg-open."
  (interactive)
  (unless args
    (setq args (f-full (s-replace "'" "\\'"
                                  (or path (if (eq major-mode 'dired-mode)
                                               (dired-get-file-for-visit)
                                             (buffer-file-name)))))))
  (let ((command (apply 'format command
                        (mapcar (lambda (a) (shell-quote-argument a))
                                args))))
    (message "Running: %s" command)
    (shell-command command)))


;;
;; OS-specific functions
;;

(defun os-open-in-browser ()
  "Open the current file in the browser."
  (interactive)
  (browse-url buffer-file-name))

(defun os-open-in-default-program ()
  "Open the current file (or selected file in dired) with xdg-open."
  (interactive)
  (doom-open-with "xdg-open '%s'"))


;;
;; Plugins
;;

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations
        '(("\\.\\(pdf\\|jpe?g\\|gif\\|docx?\\|pptx?\\|xlsx?\\|zip\\|tar\\(\\.gz\\)?\\|rar\\)$"
           "xdg-open" (file)))))

(provide 'core-os-linux)
;;; core-os-linux.el ends here
