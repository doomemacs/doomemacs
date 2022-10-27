;;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

(use-package! highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character)
  :config
  ;; HACK: If this package is loaded too early (by the user, and in terminal
  ;;   Emacs), then `highlight-indent-guides-auto-set-faces' will have been
  ;;   called much too early to set its faces correctly. To get around this, we
  ;;   need to call it again, but at a time when I can ensure a frame exists an
  ;;   the current theme is loaded.
  (when (doom-context-p 'init)
    (add-hook 'doom-first-buffer-hook #'highlight-indent-guides-auto-set-faces))

  ;; `highlight-indent-guides' breaks when `org-indent-mode' is active
  (add-hook! 'org-mode-local-vars-hook
    (defun +indent-guides-disable-maybe-h ()
      (and highlight-indent-guides-mode
           (bound-and-true-p org-indent-mode)
           (highlight-indent-guides-mode -1)))))
