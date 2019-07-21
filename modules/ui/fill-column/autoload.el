;;; ui/fill-column/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-hook! (text-mode prog-mode conf-mode) #'hl-fill-column-mode)
