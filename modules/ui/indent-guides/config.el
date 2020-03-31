;;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

(use-package! highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (defun +indent-guides-init-faces-h ()
    (when (display-graphic-p)
      (highlight-indent-guides-auto-set-faces)))

  ;; HACK `highlight-indent-guides' calculates its faces from the current theme,
  ;;      but is unable to do so properly in terminal Emacs, where it only has
  ;;      access to 256 colors. So if the user uses a daemon we must wait for
  ;;      the first graphical frame to be available to do.
  (add-hook (if (daemonp)
                'server-after-make-frame-hook
              'doom-load-theme-hook)
            #'+indent-guides-init-faces-h)

  ;; `highlight-indent-guides' breaks when `org-indent-mode' is active
  (add-hook! 'org-indent-mode-hook
    (defun +indent-guides-disable-maybe-h ()
      (when highlight-indent-guides-mode
        (highlight-indent-guides-mode -1)))))
