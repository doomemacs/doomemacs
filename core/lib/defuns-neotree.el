;;; defuns-neotree.el
;; for ../core-project.el

;;;###autoload
(defun narf/neotree-open (&optional dir)
  (interactive)
  (neotree-dir (or dir (narf/project-root))))

;;;###autoload
(defun narf/neotree-toggle ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (narf/neotree-open)))

;;;###autoload
(defun narf/neotree-find ()
  (interactive)
  (unless (neo-global--window-exists-p)
    (save-excursion (narf/neotree-open)))
  (neotree-find))

;;;###autoload
(defun narf|neotree-close-on-window-change ()
  "Close neotree to prevent ensuing mindow buggery."
  (unless (and (neo-global--window-exists-p)
               (eq (current-buffer) (neo-global--get-buffer)))
    (neotree-hide)))

;;;###autoload
(defun narf*neo-buffer-fold-symbol (name)
  "Custom hybrid ascii theme with leading whitespace."
  (let ((n-insert-symbol (lambda (n)
                           (neo-buffer--insert-with-face
                            n 'neo-expand-btn-face))))
    (or (and (eq name 'open)  (funcall n-insert-symbol "- "))
        (and (eq name 'close) (funcall n-insert-symbol "+ "))
        (and (eq name 'leaf)  (funcall n-insert-symbol "  ")))))

(provide 'defuns-neotree)
;;; defuns-neotree.el ends here
