;;; feature/syntax-checker/autoload.el -*- lexical-binding: t; -*-

(defun +syntax-checker-show-popup (errors)
  "TODO"
  (if (and EMACS26+
           (featurep! +childframe)
           (display-graphic-p))
      (flycheck-posframe-show-posframe errors)
    (flycheck-popup-tip-show-popup errors)))

(defun +syntax-checker-cleanup-popup ()
  "TODO"
  (when (display-graphic-p)
    (flycheck-popup-tip-delete-popup)))

;;;###autoload
(define-minor-mode +syntax-checker-popup-mode
  "TODO"
  :lighter nil
  :group 'doom
  (require 'flycheck-popup-tip)
  (let ((hooks '(post-command-hook focus-out-hook)))
    (cond
     ;; Use our display function and remember the old one but only if we haven't
     ;; yet configured it, to avoid activating twice.
     ((and +syntax-checker-popup-mode
           (not (eq flycheck-display-errors-function
                    #'+syntax-checker-show-popup)))
      (setq flycheck-popup-tip-old-display-function
            flycheck-display-errors-function
            flycheck-display-errors-function
            #'+syntax-checker-show-popup)
      (dolist (hook hooks)
        (add-hook hook #'+syntax-checker-cleanup-popup nil t)))
     ;; Reset the display function and remove ourselves from all hooks but only
     ;; if the mode is still active.
     ((and (not +syntax-checker-popup-mode)
           (eq flycheck-display-errors-function
               #'+syntax-checker-show-popup))
      (setq flycheck-display-errors-function
            flycheck-popup-tip-old-display-function
            flycheck-popup-tip-old-display-function nil)
      (dolist (hook hooks)
        (remove-hook hook '+syntax-checker-cleanup-popup t))))))
