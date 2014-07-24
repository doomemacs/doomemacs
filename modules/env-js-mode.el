
(require-packages '(tern tern-auto-complete))

(setq tern-ac-on-dot nil)

;; replace auto-complete with tern-ac-complete only in js-mode
(add-hook 'js-mode-hook
		  (lambda ()
			(evil-define-key 'insert ac-mode-map (kbd "C-SPC") 'tern-ac-complete)
			(tern-mode t)
			(tern-ac-setup)
			))

;; Let flycheck handle parse errors
;; (setq-default js2-show-parse-errors nil)
;; (setq-default js2-strict-missing-semi-warning nil)
;; (setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

;; (defun js2-parse-jshintrc ()
;;   "This looks recursively up for a .jshintrc and extracts the
;; globals from it to add them to js2-additional-externs."
;;   (let* ((jshintrc (expand-file-name ".jshintrc" (locate-dominating-file default-directory ".jshintrc")))
;;          (json (and jshintrc (json-read-file jshintrc)))
;;          (globals (and json (cdr (assq 'globals json)))))
;;     (when globals
;;       (setq js2-additional-externs
;;             (append
;; 				(mapcar (lambda (pair) (symbol-name (car pair))) globals)
;; 				js2-additional-externs
;;             )
;;       )
;;       (js2-reparse t)
;;     )
;;   )
;; )
;; (add-hook 'js2-init-hook 'js2-parse-jshintrc)


;;
(provide 'env-js-mode)
