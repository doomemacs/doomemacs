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

;;;###autoload (autoload '+evil:inner-url-txtobj "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:inner-url-txtobj (count &optional _beg _end type)
  "Text object to select the inner url at point.

This excludes the protocol and querystring."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     (save-excursion
       (goto-char beg)
       (re-search-forward "://" end t))
     (save-excursion
       (goto-char end)
       (- (if-let (pos (re-search-backward "[?#]" beg t))
              pos
            end)
          (if (evil-visual-state-p)
              1
            0)))
     type)))

;;;###autoload (autoload '+evil:outer-url-txtobj "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:outer-url-txtobj (count &optional _beg _end type)
  "Text object to select the whole url at point."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     beg (- end (if (evil-visual-state-p) 1 0))
     type)))
