;;; editor/snippets/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-yas-minor-mode! (modes)
  "Register minor MODES (one mode symbol or a list of them) with yasnippet so it
can have its own snippets category, if the folder exists."
  (dolist (mode (doom-enlist modes))
    (let ((fn (intern (format "+snippets-register-%s-h" mode))))
      (fset fn (lambda () (yas-activate-extra-mode mode)))
      (add-hook (intern (format "%s-hook" mode)) fn))))
