;;; lang/web/autoload/lsp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +lsp-css|css-mode ()
  "Only enable in strictly `css-mode'.
`css-mode-hook'  fires for `scss-mode' because `scss-mode' is derived from `css-mode'."
  (when (eq major-mode 'css-mode)
    (condition-case nil
        (lsp-css-enable)
      (user-error nil))))

;;;###autoload
(defun +lsp-css|scss-mode ()
  "Only enable in strictly sass-/scss-mode, not ANY. OTHER. MODES."
  (when (memq major-mode '(sass-mode scss-mode))
    (condition-case nil
        (lsp-scss-enable)
      (user-error nil))))

;;;###autoload
(defun +lsp-css|less-mode ()
  "Only enable in strictly `less-css-mode', not ANY. OTHER. MODE."
  (when (eq major-mode 'less-css-mode)
    (condition-case nil
        (lsp-less-enable)
      (user-error nil))))
