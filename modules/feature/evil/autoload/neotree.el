;;; feature/evil/autoload/neotree.el

;;;###autoload
(defun +evil/neotree ()
  "Toggle the neotree window."
  (interactive)
  (let ((in-neotree (and (neo-global--window-exists-p)
                         (window-live-p neo-global--buffer)
                         (eq (current-buffer) neo-global--buffer)))
        (path buffer-file-name))
    (if in-neotree
        (neotree-hide)
      (let ((project-root (doom-project-root)))
        (unless (and (neo-global--window-exists-p)
                     (f-same? (neo-global--with-buffer neo-buffer--start-node) project-root))
          (neotree-dir project-root))
        (neotree-find path project-root)))))

