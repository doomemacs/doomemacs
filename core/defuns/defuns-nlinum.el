;;; defuns-nlinum.el

;;;###autoload
(defun doom/nlinum-toggle ()
  (interactive)
  (if (bound-and-true-p nlinum-mode)
      (doom|nlinum-disable)
    (doom|nlinum-enable)))

;;;###autoload
(defun doom|nlinum-enable ()
  (nlinum-mode +1)
  (add-hook 'post-command-hook 'doom|nlinum-hl-line nil t)
  (doom|nlinum-unhl-line))

;;;###autoload
(defun doom|nlinum-disable ()
  (nlinum-mode -1)
  (remove-hook 'post-command-hook 'doom|nlinum-hl-line t)
  (doom|nlinum-unhl-line))

;;;###autoload
(defun doom|nlinum-unhl-line ()
  "Unhighlight line number"
  (when doom--hl-nlinum-overlay
    (let* ((disp (get-text-property
                  0 'display (overlay-get doom--hl-nlinum-overlay 'before-string)))
           (str (nth 1 disp)))
      (put-text-property 0 (length str) 'face 'linum str)
      (setq doom--hl-nlinum-overlay nil
            doom--hl-nlinum-line nil)
      disp)))

;;;###autoload
(defun doom|nlinum-hl-line (&optional line)
  "Highlight line number"
  (let ((line-no (or line (string-to-number (format-mode-line "%l")))))
    (if (and nlinum-mode (not (eq line-no doom--hl-nlinum-line)))
        (let* ((pbol (if line
                         (save-excursion (goto-char 1)
                                         (forward-line line-no)
                                         (line-beginning-position))
                       (line-beginning-position)))
               (peol (1+ pbol))
               (max (point-max)))
          ;; Handle EOF case
          (when (>= peol max)
            (setq peol max))
          (jit-lock-fontify-now pbol peol)
          (let ((ov (--first (overlay-get it 'nlinum) (overlays-in pbol peol))))
            (when ov
              (doom|nlinum-unhl-line)
              (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
                (put-text-property 0 (length str) 'face 'linum-highlight-face str)
                (setq doom--hl-nlinum-overlay ov
                      doom--hl-nlinum-line line-no))))))))

(provide 'defuns-nlinum)
;;; defuns-nlinum.el ends here
