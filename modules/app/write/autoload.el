;;; app/write/autoload.el

(defvar-local +write--buffer-mode nil
  "TODO")

;;;###autoload
(define-minor-mode +write-mode
  :init-value nil
  :keymap nil
  (let ((arg  (if +write-mode +1 -1))
        (iarg (if +write-mode -1 +1)))
    (when (and (featurep 'doom-themes)
               (not +write--buffer-mode)
               +write-mode)
      (setq +write--buffer-mode doom-buffer-mode))
    (text-scale-set (if +write-mode 2 0))
    (nlinum-mode iarg)
    (setq-local visual-fill-column-center-text +write-mode)
    (visual-fill-column-mode arg)
    (visual-line-mode arg)
    (when (eq major-mode 'org-mode)
      (+org-pretty-mode arg))
    (setq line-spacing (if +write-mode 4))))

