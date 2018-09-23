;;; lang/web/autoload/lsp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +web|init-lsp-css ()
  (condition-case _
      (pcase major-mode
        (`css-mode (lsp-css-enable))
        ((or 'scss-mode 'sass-mode) (lsp-scss-enable))
        (`less-css-mode (lsp-less-enable)))
    (user-error)))

;;;###autoload
(defun +web|init-lsp-html ()
  (condition-case _
      (pcase major-mode (`html-mode (lsp-html-enable)))
    (user-error)))

