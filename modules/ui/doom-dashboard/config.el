;;; ui/doom-dashboard/config.el -*- lexical-binding: t; -*-

(use-package! dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

;;TODO add general bindings

  (setq dashboard-banner-logo-title "I must have been stress-testing our production server.") ;;TODO add random phrases
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner ;;TODO add custom logo
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))

  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))
