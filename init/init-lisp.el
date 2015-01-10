;; Elisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook! 'after-save-hook
             (if (file-exists-p (concat buffer-file-name "c"))
                 (delete-file (concat buffer-file-name "c")))))

(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(evil-define-operator my:elisp-eval (beg end)
  :move-point nil
  (interactive "<r>")
  (cond ((and beg end)
         (eval-region beg end))
        (t (eval-buffer))))

;; Real go-to-definition for elisp
(bind 'motion emacs-lisp-mode-map "gd"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function func)))))

;; Go-to-definition in other buffer
(bind 'motion emacs-lisp-mode-map "gD"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function-other-window func)))))

(bind 'motion emacs-lisp-mode-map "g r" 'my:elisp-eval)

;; TODO Add clojure support
;; TODO Add scheme support


(provide 'init-lisp)
;;; init-elisp.el ends here
