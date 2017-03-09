;;; app/write/autoload.el

(defvar-local +write--buffer-mode nil
  "TODO")

;;;###autoload
(define-minor-mode +write-mode
  :init-value nil
  :keymap nil
  (let ((arg (if +write-mode +1 -1)))
    (when (and (featurep 'doom-themes)
               (not +write--buffer-mode)
               +write-mode)
      (setq +write--buffer-mode doom-buffer-mode))
    (text-scale-set (if +write-mode 1.5 0))
    (visual-fill-column-mode arg)
    (setq line-spacing (if +write-mode 6))))
