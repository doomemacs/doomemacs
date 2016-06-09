;;; defuns-nlinum.el

;;;###autoload
(defun doom/nlinum-toggle ()
  (interactive)
  (if (bound-and-true-p nlinum-mode)
      (doom|nlinum-disable)
    (doom|nlinum-enable)))

;;;###autoload
(defun doom|nlinum-enable (&rest _)
  (nlinum-mode +1)
  (add-hook 'post-command-hook 'doom|nlinum-hl-line nil t)
  (doom--nlinum-unhl-line))

;;;###autoload
(defun doom|nlinum-disable (&rest _)
  (nlinum-mode -1)
  (remove-hook 'post-command-hook 'doom|nlinum-hl-line t)
  (doom--nlinum-unhl-line))

(defun doom--nlinum-unhl-line ()
  "Unhighlight line number"
  (when doom--hl-nlinum-overlay
    (let* ((disp (get-text-property
                  0 'display (overlay-get doom--hl-nlinum-overlay 'before-string)))
           (str (nth 1 disp)))
      (put-text-property 0 (length str) 'face 'linum str)
      (setq doom--hl-nlinum-overlay nil)
      disp)))

;;;###autoload
(defun doom|nlinum-hl-line ()
  "Highlight line number"
  (let* ((pbol (line-beginning-position))
         (peol (1+ pbol))
         (max (point-max)))
    ;; Handle EOF case
    (when (>= peol max)
      (setq peol max))
    (jit-lock-fontify-now pbol peol)
    (let ((ov (--first (overlay-get it 'nlinum) (overlays-in pbol peol))))
      (doom--nlinum-unhl-line)
      (when ov
        (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
          (put-text-property 0 (length str) 'face 'doom-nlinum-highlight str)
          (setq doom--hl-nlinum-overlay ov))))))

(provide 'defuns-nlinum)
;;; defuns-nlinum.el ends here
