;;; defuns-evil.el

;;;###autoload (autoload 'narf:evil-open-folds "defuns-evil")
(evil-define-command narf/evil-open-folds (count)
  "Instead of `evil-open-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (if count (hs-hide-level count) (evil-open-folds)))

;;;###autoload (autoload 'narf:evil-open-folds "defuns-evil")
(evil-define-command narf/evil-close-folds (count)
  "Instead of `evil-close-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (if count (hs-hide-level count) (evil-close-folds)))

;;;; Ace Jump ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/winterTTr/ace-jump-mode/issues/23
;;;###autoload (autoload 'narf:evil-ace-jump-two-chars "defuns-evil")
(evil-define-motion narf/evil-ace-jump-two-chars (count)
  :type exclusive
  :repeat abort
  (evil-without-repeat
    (evil-enclose-ace-jump-for-motion
      (call-interactively 'ace-jump-two-chars-mode))))

;;;###autoload
(defun narf/evil-visual-line-state-p ()
  "Returns non-nil if in visual-line mode, nil otherwise."
  (and (evil-visual-state-p)
       (eq (evil-visual-type) 'line)))


(provide 'defuns-evil)
;;; defuns-evil.el ends here
