;;; defuns-neotree.el
;; for ../core-project.el

;;;###autoload
(defun narf/neotree ()
  "Toggle the neotree window"
  (interactive)
  (let ((in-neotree (and (neo-global--window-exists-p)
                         (window-live-p neo-global--buffer)
                         (eq (current-buffer) neo-global--buffer))))
    (if in-neotree
        (neotree-hide)
      (let ((project-root (narf/project-root)))
        (unless (and (neo-global--window-exists-p)
                     (f-same? (neo-global--with-window neo-buffer--start-node) project-root))
          (neotree-dir project-root)))
      (neotree-find))))

;;;###autoload
(defun narf|neotree-close-on-window-change (&rest _)
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
