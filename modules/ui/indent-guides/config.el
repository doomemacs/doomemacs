;;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

(use-package! highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-suppress-auto-error t)
  :config
  (defun +indent-guides-init-faces-h (&rest _)
    (when (display-graphic-p)
      (highlight-indent-guides-auto-set-faces)))

  ;; HACK `highlight-indent-guides' calculates its faces from the current theme,
  ;;      but is unable to do so properly in terminal Emacs, where it only has
  ;;      access to 256 colors. So if the user uses a daemon we must wait for
  ;;      the first graphical frame to be available to do.
  (add-hook 'doom-load-theme-hook #'+indent-guides-init-faces-h)
  (when doom-theme
    (+indent-guides-init-faces-h))

  ;; `highlight-indent-guides' breaks when `org-indent-mode' is active
  (add-hook! 'org-mode-local-vars-hook
    (defun +indent-guides-disable-maybe-h ()
      (and highlight-indent-guides-mode
           (bound-and-true-p org-indent-mode)
           (highlight-indent-guides-mode -1)))))
