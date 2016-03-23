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
    (remove-hook 'post-command-hook 'narf|nlinum-hl-line)
    (narf|nlinum-unhl-line))

(provide 'defuns-nlinum)
;;; defuns-nlinum.el ends here
