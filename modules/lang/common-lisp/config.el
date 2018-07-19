;;; lang/common-lisp/config.el -*- lexical-binding: t; -*-

(after! sly
  (setq inferior-lisp-program "sbcl")

  (set-popup-rule! "^\\*sly" :quit nil :ttl nil)
  (set-repl-handler! 'lisp-mode #'sly-mrepl)
  (set-lookup-handlers! 'lisp-mode
    :definition #'sly-edit-definition
    :documentation #'sly-describe-symbol)

  (defun +common-lisp|cleanup-sly-maybe ()
    "Kill processes and leftover buffers when killing the last sly buffer."
    (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'sly-mode buf)
                             (get-buffer-window buf))
                     return t)
      (dolist (conn (sly--purge-connections))
        (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
      (let (kill-buffer-hook kill-buffer-query-functions)
        (mapc #'kill-buffer
              (cl-loop for buf in (delq (current-buffer) (buffer-list))
                       if (buffer-local-value 'sly-mode buf)
                       collect buf)))))

  (defun +common-lisp|init-sly ()
    "Attempt to auto-start sly when opening a lisp buffer."
    (cond ((sly-connected-p))
          ((executable-find inferior-lisp-program)
           (let ((sly-auto-start 'always))
             (sly-auto-start)
             (add-hook 'kill-buffer-hook #'+common-lisp|cleanup-sly-maybe nil t)))
          ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                    inferior-lisp-program))))
  (add-hook 'sly-mode-hook #'+common-lisp|init-sly)

  ;; evil integration
  (when (featurep! :feature evil +everywhere)
    (add-hook 'sly-popup-buffer-mode-hook #'evil-normalize-keymaps)
    (unless evil-move-beyond-eol
      (advice-add #'sly-eval-last-expression :around #'+common-lisp*sly-last-sexp)
      (advice-add #'sly-pprint-eval-last-expression :around #'+common-lisp*sly-last-sexp)
      (advice-add #'sly-eval-print-last-expression :around #'+common-lisp*sly-last-sexp)
      (advice-add #'sly-eval-last-expression-in-repl :around #'+common-lisp*sly-last-sexp))
    (set-evil-initial-state!
      '(sly-db-mode sly-inspector-mode sly-popup-buffer-mode sly-xref-mode)
      'normal)
    (evil-define-key 'normal sly-parent-map
      (kbd "C-t") #'sly-pop-find-definition-stack)
    (evil-define-key 'normal sly-db-mode-map
      (kbd "RET") 'sly-db-default-action
      (kbd "C-m") 'sly-db-default-action
      [return] 'sly-db-default-action
      [mouse-2]  'sly-db-default-action/mouse
      [follow-link] 'mouse-face
      "\C-i" 'sly-db-cycle
      "g?" 'describe-mode
      "S" 'sly-db-show-source
      "e" 'sly-db-eval-in-frame
      "d" 'sly-db-pprint-eval-in-frame
      "D" 'sly-db-disassemble
      "i" 'sly-db-inspect-in-frame
      "gj" 'sly-db-down
      "gk" 'sly-db-up
      (kbd "C-j") 'sly-db-down
      (kbd "C-k") 'sly-db-up
      "]" 'sly-db-details-down
      "[" 'sly-db-details-up
      (kbd "C-S-j") 'sly-db-details-down
      (kbd "C-S-k") 'sly-db-details-up
      "gg" 'sly-db-beginning-of-backtrace
      "G" 'sly-db-end-of-backtrace
      "t" 'sly-db-toggle-details
      "gr" 'sly-db-restart-frame
      "I" 'sly-db-invoke-restart-by-name
      "R" 'sly-db-return-from-frame
      "c" 'sly-db-continue
      "s" 'sly-db-step
      "n" 'sly-db-next
      "o" 'sly-db-out
      "b" 'sly-db-break-on-return
      "a" 'sly-db-abort
      "q" 'sly-db-quit
      "A" 'sly-db-break-with-system-debugger
      "B" 'sly-db-break-with-default-debugger
      "P" 'sly-db-print-condition
      "C" 'sly-db-inspect-condition
      "g:" 'sly-interactive-eval
      "0" 'sly-db-invoke-restart-0
      "1" 'sly-db-invoke-restart-1
      "2" 'sly-db-invoke-restart-2
      "3" 'sly-db-invoke-restart-3
      "4" 'sly-db-invoke-restart-4
      "5" 'sly-db-invoke-restart-5
      "6" 'sly-db-invoke-restart-6
      "7" 'sly-db-invoke-restart-7
      "8" 'sly-db-invoke-restart-8
      "9" 'sly-db-invoke-restart-9)
    (evil-define-key 'normal sly-inspector-mode-map
      [return] 'sly-inspector-operate-on-point
      (kbd "C-m") 'sly-inspector-operate-on-point
      [mouse-1] 'sly-inspector-operate-on-click
      [mouse-2] 'sly-inspector-operate-on-click
      [mouse-6] 'sly-inspector-pop
      [mouse-7] 'sly-inspector-next
      "gk" 'sly-inspector-pop
      (kbd "C-k") 'sly-inspector-pop
      "gj" 'sly-inspector-next
      "j" 'sly-inspector-next
      "k" 'sly-inspector-previous-inspectable-object
      "K" 'sly-inspector-describe
      "p" 'sly-inspector-pprint
      "e" 'sly-inspector-eval
      "h" 'sly-inspector-history
      "gr" 'sly-inspector-reinspect
      "gv" 'sly-inspector-toggle-verbose
      "\C-i" 'sly-inspector-next-inspectable-object
      [(shift tab)] 'sly-inspector-previous-inspectable-object ; Emacs translates S-TAB
      [backtab] 'sly-inspector-previous-inspectable-object     ; to BACKTAB on X.
      "." 'sly-inspector-show-source
      "gR" 'sly-inspector-fetch-all
      "q" 'sly-inspector-quit)
    (evil-define-key 'normal sly-mode-map
      (kbd "C-t") 'sly-pop-find-definition-stack)
    (evil-define-key 'normal sly-popup-buffer-mode-map
      "q" 'quit-window
      (kbd "C-t") 'sly-pop-find-definition-stack)
    (evil-define-key 'normal sly-xref-mode-map
      (kbd "RET") 'sly-goto-xref
      (kbd "S-<return>") 'sly-goto-xref
      "go" 'sly-show-xref
      "gj" 'sly-xref-next-line
      "gk" 'sly-xref-prev-line
      (kbd "C-j") 'sly-xref-next-line
      (kbd "C-k") 'sly-xref-prev-line
      "]" 'sly-xref-next-line
      "[" 'sly-xref-prev-line
      "gr" 'sly-recompile-xref
      "gR" 'sly-recompile-all-xrefs
      "r" 'sly-xref-retract)))
