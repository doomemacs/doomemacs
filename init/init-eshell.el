;; (evil-set-initial-state 'eshell-mode 'emacs)

;; (push '("*eshell*" :position left :width 80 :stick t) popwin:special-display-config)

;; ;; eshell
;; (setq eshell-directory-name (concat TMP-DIR "eshell"))
;; (setq eshell-scroll-to-bottom-on-input 'all)
;; (setq eshell-buffer-shorthand t)


;; ;; em-alias
;; (setq eshell-aliases-file (concat TMP-DIR ".eshell-aliases"))


;; ;; em-glob
;; (setq eshell-glob-case-insensitive t)
;; (setq eshell-error-if-no-glob t)


;; ;; em-hist
;; (setq eshell-history-size 1024)


;; ;; plan 9 smart shell
;; ;; (after "esh-module"
;; ;;        (add-to-list 'eshell-modules-list 'eshell-smart)
;; ;;        (setq eshell-where-to-jump 'begin)
;; ;;        (setq eshell-review-quick-commands nil)
;; ;;        (setq eshell-smart-space-goes-to-end t))

;; (defun my-current-git-branch ()
;;   (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
;;                             when (string-match "^\*" match)
;;                             collect match))))
;;     (if (not (eq branch nil))
;;         (concat " [" (substring branch 2) "]")
;;       "")))

;; (defun my-eshell-prompt ()
;;   (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
;;           (propertize (my-current-git-branch) 'face 'font-lock-function-name-face)
;;           (propertize " $ " 'face 'font-lock-constant-face)))


;; ;; em-prompt
;; (setq eshell-prompt-function 'my-eshell-prompt)


(provide 'init-eshell)
;;; init-eshell.el ends here
