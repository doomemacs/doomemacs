;; Elisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Real go-to-definition for elisp
(bind 'motion emacs-lisp-mode-map "gd"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function func)))))

;; Go-to-definition in other buffer
(bind 'motion emacs-lisp-mode-map "gD"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function-other-window func)))))

;; TODO Add clojure support
;; TODO Add scheme support


(provide 'init-lisp)
;;; init-elisp.el ends here
