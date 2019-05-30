;;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

(def-package! highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)

  (defun +indent-guides|disable-maybe ()
    (when highlight-indent-guides-mode
      (highlight-indent-guides-mode -1)))
  ;; `highlight-indent-guides' breaks in these modes
  (add-hook 'visual-line-mode-hook #'+indent-guides|disable-maybe)
  (add-hook 'org-indent-mode-hook #'+indent-guides|disable-maybe))
