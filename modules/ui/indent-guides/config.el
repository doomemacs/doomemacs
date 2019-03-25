;;; ui/indent-guides/config.el -*- lexical-binding: t; -*-

(def-package! highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  :config
  ;; Don't display first level of indentation
  (defun +indent-guides-for-all-but-first-column (level responsive display)
    (unless (< level 1)
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function #'+indent-guides-for-all-but-first-column)

  (defun +indent-guides|disable-maybe ()
    (when highlight-indent-guides
      (highlight-indent-guides -1)))
  ;; `highlight-indent-guides' breaks in `visual-line-mode'
  (add-hook 'visual-line-mode-hook #'+indent-guides|disable-maybe)
  (add-hook 'org-indent-mode-hook #'+indent-guides|disable-maybe))
