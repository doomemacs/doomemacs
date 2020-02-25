;;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

(use-package! highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
  ;; `highlight-indent-guides' breaks in these modes
  (add-hook! 'org-indent-mode-hook
    (defun +indent-guides-disable-maybe-h ()
      (when highlight-indent-guides-mode
        (highlight-indent-guides-mode -1)))))
