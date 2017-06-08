;;; app/write/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode +write-mode
  "TODO"
  :init-value nil
  :keymap nil
  (let ((arg  (if +write-mode +1 -1))
        (iarg (if +write-mode -1 +1)))
    (text-scale-set (if +write-mode 2 0))
    (doom/toggle-line-numbers iarg)
    (setq-local visual-fill-column-center-text +write-mode)
    (visual-fill-column-mode arg)
    (visual-line-mode arg)
    (when (eq major-mode 'org-mode)
      (+org-pretty-mode arg))
    (setq line-spacing (if +write-mode 4))))

