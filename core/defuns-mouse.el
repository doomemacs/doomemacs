;;;###autoload
(defun narf/mouse-line-at-click ()
  "Determine the line number at click"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos)))))

;;;###autoload
(defun narf/mouse-select-line (event)
  "Set point as *linum-mdown-line*"
  (interactive "e")
  (mouse-select-window event)
  (goto-line (narf/mouse-line-at-click))
  (evil-visual-line)
  (setq *linum-mdown-line* (line-number-at-pos)))

;;;###autoload
(defun narf/mouse-select-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))


(provide 'defuns-mouse)
;;; defuns-mouse.el ends here
