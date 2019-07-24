;;; ui/fill-column/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'hl-fill-column-mode "hl-fill-column" nil t)

;;;###autoload
(add-hook! (text-mode prog-mode conf-mode) #'hl-fill-column-mode)
