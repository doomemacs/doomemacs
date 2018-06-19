;;; feature/snippets/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-yas-minor-mode! (modes)
  "Register minor MODES (one mode symbol or a list of them) with yasnippet so it
can have its own snippets category, if the folder exists."
  (let ((fn (intern (format "+snippets|register-%s" mode))))
    (fset fn (lambda ()
               (make-local-variable 'yas--extra-modes)
               (dolist (mode (doom-enlist modes))
                 (add-to-list 'yas--extra-modes mode nil #'eq))
               (yas--load-pending-jits)))
    (add-hook (intern (format "%s-hook" mode)) fn)))

;;;###autoload
(def-setting! :yas-minor-mode (mode)
  :obsolete set-yas-minor-mode!
  `(set-yas-minor-mode! ,mode))
