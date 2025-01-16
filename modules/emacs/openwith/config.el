;;; emacs/openwith/config.el -*- lexical-binding: t; -*-

(use-package! openwith
  :hook (emacs-startup . openwith-mode)
  :init
  (setq +openwith-extensions '("pdf" "jpg" "png" "jpeg" "mp4"))
  :config
  (when-let (cmd (cond ((featurep :system 'macos) "open")
                       ((featurep :system 'linux) "xdg-open")
                       ((featurep :system 'windows) "start")))
    (setq openwith-associations
          (list (list (openwith-make-extension-regexp +openwith-extensions)
                      cmd '(file)))))
  (advice-add #'openwith-file-handler :around
              (lambda (fn &rest args)
                (let ((process-connection-type nil))
                  (apply fn args)))))
