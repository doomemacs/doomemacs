;;; defuns-nlinum.el

;;;###autoload
(defun narf/nlinum-toggle ()
  (interactive)
  (if (bound-and-true-p nlinum-mode)
      (narf|nlinum-disable)
    (narf|nlinum-enable)))

;;;###autoload
(defun narf|nlinum-disable ()
  (nlinum-mode -1)
  (remove-hook 'post-command-hook 'narf|nlinum-hl-line t)
  (narf|nlinum-unhl-line))

;;;###autoload
(defun narf|nlinum-unhl-line ()
  "Unhighlight line number"
  (when narf--hl-nlinum-overlay
    (let* ((disp (get-text-property
                  0 'display (overlay-get narf--hl-nlinum-overlay 'before-string)))
           (str (nth 1 disp)))
      (put-text-property 0 (length str) 'face 'linum str)
      (setq narf--hl-nlinum-overlay nil
            narf--hl-nlinum-line nil)
      disp)))

;;;###autoload
(defun narf|nlinum-hl-line (&optional line)
  "Highlight line number"
  (let ((line-no (or line (string-to-number (format-mode-line "%l")))))
    (if (and nlinum-mode (not (eq line-no narf--hl-nlinum-line)))
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
          (let ((ov (-first (lambda (item) (overlay-get item 'nlinum)) (overlays-in pbol peol))))
            (when ov
              (narf|nlinum-unhl-line)
              (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
                (put-text-property 0 (length str) 'face 'linum-highlight-face str)
                (setq narf--hl-nlinum-overlay ov
                      narf--hl-nlinum-line line-no))))))))

(provide 'defuns-nlinum)
;;; defuns-nlinum.el ends here
