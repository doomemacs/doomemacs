
;; TODO: Do this later

(add-hook! 'hs-minor-mode-hook (when hs-minor-mode (hs-hide-level 3)))

;; [pedantry intensifies]
(defadvice emacs-lisp-mode (after emacs-lisp-mode-rename-modeline activate)
  (setq mode-name "Elisp"))

;; Real go-to-definition for elisp
(bind :motion :map emacs-lisp-mode-map
      "gd" (λ (let ((func (function-called-at-point)))
                (if func (find-function func))))
      "gD" (λ (let ((func (function-called-at-point)))
                (if func (find-function-other-window func)))))


(provide 'init-lisp)
;;; init-lisp.el ends here
