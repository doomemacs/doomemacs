;;; tools/flycheck/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +flycheck*popup-tip-delete-popup (orig-fn)
  "TODO"
  (when (display-graphic-p)
    (funcall orig-fn)))

;;;###autoload
(defun +flycheck*popup-tip-show-popup (orig-fn errors)
  "TODO"
  (if (and EMACS26+
           (featurep! +childframe)
           (display-graphic-p))
      (flycheck-posframe-show-posframe errors)
    (funcall orig-fn errors)))

;;;###autoload
(defun +flycheck|disable-popup-tip-for-lsp ()
  "Disable `+flycheck-popup-mode' if `lsp-ui-mode' and `lsp-ui-sideline-enable'
are non-nil."
  (when (and (bound-and-true-p lsp-ui-mode)
             lsp-ui-sideline-enable)
    (flycheck-popup-tip-mode -1)))
