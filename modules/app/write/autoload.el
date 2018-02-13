;;; app/write/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode +write-mode
  "Turns Emacs into a more comfortable writing environment and word processor."
  :init-value nil
  :keymap nil
  (setq-local visual-fill-column-center-text t)
  (when +write-text-scale
    (text-scale-set (if +write-mode 2 0)))
  (when +write-line-spacing
    (setq-local line-spacing +write-line-spacing)))

;;;###autoload
(defun +write|init-org-mode ()
  "Initializes `org-mode' specific settings for `+write-mode'."
  (when (eq major-mode 'org-mode)
    (let ((arg  (if +write-mode +1 -1)))
      (+org-pretty-mode arg)
      (org-indent-mode (if +write-mode -1 +1)))))
