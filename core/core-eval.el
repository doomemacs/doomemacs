;;; core-eval.el

;; + Running inline code + REPLs (using `quickrun' + `repl-toggle')
;; + Simple code navigation (using `dump-jump' and `imenu-list')

;; remove ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(use-package quickrun
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-shell
             quickrun-compile-only
             quickrun-replace-region)
  :init (add-hook 'quickrun/mode-hook 'linum-mode)
  :config
  (setq quickrun-focus-p nil)
  (def-popup! "*quickrun*" :align below :size 10)

  ;;; Popup hacks
  (advice-add 'quickrun :before 'doom*quickrun-close-popup)
  (advice-add 'quickrun-region :before 'doom*quickrun-close-popup)
  ;; Ensures window is scrolled to BOF
  (add-hook 'quickrun-after-run-hook 'doom|quickrun-after-run))

(use-package repl-toggle
  :commands (rtog/toggle-repl rtog/add-repl)
  :preface (defvar rtog/mode-repl-alist nil)
  :init
  (defvar doom-repl-buffer nil "The current REPL buffer.")
  (add-hook! repl-toggle-mode (evil-initialize-state 'emacs))
  :config
  (def-popup!
    (:custom (lambda (b &rest _)
               (when (and (featurep 'repl-toggle)
                          (string-prefix-p "*" (buffer-name (get-buffer b))))
                 (buffer-local-value 'repl-toggle-mode b))))
    :popup t :align below :size 16 :select t)

  (map! :map repl-toggle-mode-map
        :ei "C-n" 'comint-next-input
        :ei "C-p" 'comint-previous-input
        :ei "<down>" 'comint-next-input
        :ei "<up>"   'comint-previous-input))

(provide 'core-eval)
;;; core-eval.el ends here
