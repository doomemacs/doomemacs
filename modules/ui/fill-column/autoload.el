;;; ui/fill-column/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'hl-fill-column-mode "hl-fill-column" nil t)

;;;###autoload
;; Emacs 27 introduced `display-fill-column-indicator-mode' which should be
;; used instead of `hl-fill-column-mode'
(if EMACS27+
    (add-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
               #'display-fill-column-indicator-mode)
    (add-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
               #'hl-fill-column-mode))
