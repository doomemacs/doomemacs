;;; ui/fill-column/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defalias '+fill-column/toggle
  (if (fboundp 'display-fill-column-indicator-mode)
      #'display-fill-column-indicator-mode  ; Emacs 27 only
    (autoload 'hl-fill-column-mode "hl-fill-column" nil t)
    #'hl-fill-column-mode))

;;;###autoload
(defalias '+fill-column-enable-h #'+fill-column/toggle)

;;;###autoload
(add-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
           #'+fill-column-enable-h)
