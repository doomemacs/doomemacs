;;; checkers/syntax/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-next-checker! (mode checker next &optional append)
  "TODO"
  (let ((fn (intern (format "+syntax--init-checkers-for-%s-h" mode))))
    (fset fn
          (lambda ()
            (if (not (bound-and-true-p flycheck-mode))
                (add-hook 'flycheck-mode-hook fn 'append 'local)
              (flycheck-add-next-checker checker next append)
              (remove-hook 'flycheck-mode-hook fn 'local))))
    (add-hook (intern (format "%s-hook" mode)) fn)))

;;;###autoload
(defun +syntax-init-popups-h ()
  "Activate `flycheck-posframe-mode' if available and in GUI Emacs.
Activate `flycheck-popup-tip-mode' otherwise.
Do nothing if `lsp-ui-mode' is active and `lsp-ui-sideline-enable' is non-nil."
  (unless (and (bound-and-true-p lsp-ui-mode)
               lsp-ui-sideline-enable)
    (if (and (fboundp 'flycheck-posframe-mode)
             (display-graphic-p))
        (flycheck-posframe-mode +1)
      (flycheck-popup-tip-mode +1))))
