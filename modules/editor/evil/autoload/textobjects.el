;;; editor/evil/autoload/textobjects.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+evil:whole-buffer-txtobj "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (evil-range (point-min) (point-max) type))

;;;###autoload (autoload '+evil:defun-txtobj "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:defun-txtobj (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'defun)
    (evil-range beg end type)))
