;;; tools/flycheck/autoload.el -*- lexical-binding: t; -*-

(defun +flycheck-show-popup (errors)
  "TODO"
  (if (and EMACS26+
           (featurep! +childframe)
           (display-graphic-p))
      (flycheck-posframe-show-posframe errors)
    (flycheck-popup-tip-show-popup errors)))

(defun +flycheck-cleanup-popup ()
  "TODO"
  (when (display-graphic-p)
    (flycheck-popup-tip-delete-popup)))

;;;###autoload
(define-minor-mode +flycheck-popup-mode
  "TODO"
  :lighter nil
  :group 'doom
  (require 'flycheck-popup-tip)
  (let ((hooks '(post-command-hook focus-out-hook)))
    (cond
     ;; Use our display function and remember the old one but only if we haven't
     ;; yet configured it, to avoid activating twice.
     ((and +flycheck-popup-mode
           (not (eq flycheck-display-errors-function
                    #'+flycheck-show-popup)))
      (setq flycheck-popup-tip-old-display-function
            flycheck-display-errors-function
            flycheck-display-errors-function
            #'+flycheck-show-popup)
      (dolist (hook hooks)
        (add-hook hook #'+flycheck-cleanup-popup nil t)))
     ;; Reset the display function and remove ourselves from all hooks but only
     ;; if the mode is still active.
     ((and (not +flycheck-popup-mode)
           (eq flycheck-display-errors-function
               #'+flycheck-show-popup))
      (setq flycheck-display-errors-function
            flycheck-popup-tip-old-display-function
            flycheck-popup-tip-old-display-function nil)
      (dolist (hook hooks)
        (remove-hook hook '+flycheck-cleanup-popup t))))))

;;;###autoload
(defun +flycheck|disable-popup-mode-for-lsp ()
  "Disable `+flycheck-popup-mode' if `lsp-ui-mode' and `lsp-ui-sideline-enable'
are non-nil."
  (when (and (bound-and-true-p lsp-ui-mode)
             lsp-ui-sideline-enable)
    (+flycheck-popup-mode -1)))
