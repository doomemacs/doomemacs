;;; defuns-iedit.el

;;;###autoload
(defun narf:iedit-restrict-to-region (&optional beg end)
  (interactive)
  (if (iedit-current-occurrence-string)
      (let ((current-prefix-arg '(4))
            (beg (or beg (region-beginning)))
            (end (or end (region-end))))
        (iedit-done)
        (call-interactively 'iedit-mode)
        (save-excursion (iedit-restrict-region beg end))
        (evil-previous-line))
    (call-interactively 'evil-ret)))

;;;###autoload
(defun narf/mark-and-prev ()
  (interactive)
  (narf/mark-and-next t))

;;;###autoload
(defun narf/mark-and-next (&optional backwards-p)
  (interactive)
  (let ((beg evil-visual-beginning)
        (end evil-visual-end)
        (last-pos (point)))
    (unless (bound-and-true-p evil-iedit-state-local-minor-mode)
      (save-excursion
        (funcall (intern (format "evil-visualstar/begin-search-%s" (if backwards-p "backward" "forward")))
                 beg end))
      (when backwards-p
        (goto-char beg))
      (evil-visual-make-region beg end)
      (call-interactively 'evil-iedit-state/iedit-mode)
      (save-excursion
        (narf:iedit-restrict-to-region beg end)))
    (evil-ex-find-next nil (if backwards-p 'backward 'forward) t)
    ;; (evil-ex-search-next)
    (evil-ex-nohighlight)
    (when (and (iedit-find-current-occurrence-overlay)
               (if backwards-p t (not (= last-pos (point)))))
      (iedit-toggle-selection))))

(provide 'defuns-iedit)
;;; defuns-iedit.el ends here
