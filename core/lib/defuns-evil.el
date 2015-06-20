;;; defuns-evil.el
;; for ../core-evil.el

;;;###autoload (autoload 'narf:evil-open-folds "defuns-evil" nil t)
(evil-define-command narf/evil-open-folds (count)
  "Instead of `evil-open-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if count (hs-hide-level count) (evil-open-folds)))

;;;###autoload (autoload 'narf:evil-open-folds "defuns-evil" nil t)
(evil-define-command narf/evil-close-folds (count)
  "Instead of `evil-close-folds'. Accepts COUNT for dictating fold level."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode 1))
  (if count (hs-hide-level count) (evil-close-folds)))

;;;; Ace Jump ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/winterTTr/ace-jump-mode/issues/23
;;;###autoload (autoload 'narf:evil-ace-jump-two-chars "defuns-evil" nil t)
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

;;;###autoload
(defun narf:iedit-restrict-to-region ()
  (interactive)
  (if (iedit-current-occurrence-string)
      (let ((current-prefix-arg '(4)))
        (iedit-done)
        (call-interactively 'iedit-mode)
        (save-excursion (iedit-restrict-region (region-beginning) (region-end)))
        (evil-previous-line))
    (call-interactively 'evil-ret)))

;;;###autoload
(defun narf*evil-exchange-off ()
  (when evil-exchange--overlays
    (evil-exchange-cancel)))

;;;###autoload
(defun narf/evil-surround-escaped ()
  "Escaped surround characters."
    (let* ((char (string (read-char "\\")))
           (pair (cond ((string-match "[]})[{(]" char)
                        (let ((-pair (cdr (assoc (string-to-char char) evil-surround-pairs-alist))))
                          `(,(car -pair) . ,(cdr -pair))))
                       (t
                        `(,char . ,char))))
           (format (if (sp-point-in-string) "\\\\%s" "\\%s")))
      (cons (format format (car pair))
            (format format (cdr pair)))))

(provide 'defuns-evil)
;;; defuns-evil.el ends here
