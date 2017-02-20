;;; private/hlissner/autoload/evil.el

;;;###autoload (autoload '+hlissner:multi-next-line "private/hlissner/autoload/evil" nil t)
(evil-define-motion +hlissner:multi-next-line (count)
  "Move down 6 lines."
  :type line
  (let ((line-move-visual visual-line-mode))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload '+hlissner:multi-previous-line "private/hlissner/autoload/evil" nil t)
(evil-define-motion +hlissner:multi-previous-line (count)
  "Move up 6 lines."
  :type line
  (let ((line-move-visual visual-line-mode))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload (autoload '+hlissner:cd "private/hlissner/autoload/evil" nil t)
(evil-define-command +hlissner:cd ()
  "Change `default-directory' with `cd'."
  (interactive "<f>")
  (cd input))

