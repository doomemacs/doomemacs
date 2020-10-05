;;; ui/fill-column/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'hl-fill-column-mode "hl-fill-column" nil t)

;;;###autoload
;; Emacs 27 introduced `display-fill-column-indicator-mode' which should be
;; used instead of `hl-fill-column-mode'
(defun +fill-column-enable-h (&optional arg)
  (interactive "p")
  (if (fboundp 'display-fill-column-indicator-mode)
      (display-fill-column-indicator-mode arg)
    (hl-fill-column-mode arg)))

;;;###autoload
(add-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
           #'+fill-column-enable-h)
