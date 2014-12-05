(provide 'init-elisp)

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'after-save-hook 'remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook! 'emacs-lisp-mode-hook
           (setq my-run-code-func 'eval-buffer
                 my-run-code-region-func 'eval-region))

;; Real go-to-definition for elisp
(bind 'motion emacs-lisp-mode-map "gd"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function func)))))

;; Go-to-definition in other buffer
(bind 'motion emacs-lisp-mode-map "gD"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function-other-window func)))))
