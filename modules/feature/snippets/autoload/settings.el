;;; feature/snippets/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-yas-minor-mode! (mode)
  "Register a minor MODE with yasnippet so it can have its own snippets
category, if the folder exists."
  (after! yasnippet
    (let ((fn (intern (format "+snippets--register-%s" mode))))
      (fset fn (lambda () (+snippets|enable-project-modes mode)))
      (add-hook (intern (format "%s-hook" mode)) fn))))

;;;###autoload
(def-setting! :yas-minor-mode (mode)
  :obsolete set-yas-minor-mode!
  `(set-yas-minor-mode! ,mode))
