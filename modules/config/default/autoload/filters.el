;;; config/default/autoload/filters.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +yas-active-p ()
  "Return t if we are in a YASnippet field."
  (memq (bound-and-true-p yas--active-field-overlay)
      (overlays-in (1- (point)) (1+ (point)))))
