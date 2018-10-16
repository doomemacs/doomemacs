;;; lang/common-lisp/config.el -*- lexical-binding: t; -*-

(defvar inferior-lisp-program "sbcl")
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)


(after! sly
  (setq sly-mrepl-history-file-name (concat doom-cache-dir "sly-mrepl-history")
        sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix)

  (set-popup-rules!
    '(("^\\*sly-mrepl"       :vslot 2 :quit nil :ttl nil)
      ("^\\*sly-compilation" :vslot 3 :ttl nil)
      ("^\\*sly-traces"      :vslot 4 :ttl nil)))

  ;; Do not display debugger or inspector buffers in a popup window.
  ;; These buffers are meant to be displayed with sufficient vertical space.
  (set-popup-rule! "^\\*sly-\\(db\\|inspector\\)" :ignore t)

  (set-repl-handler! 'lisp-mode #'sly-mrepl)
  (set-eval-handler! 'lisp-mode #'sly-eval-region)
  (set-lookup-handlers! 'lisp-mode
    :definition #'sly-edit-definition
    :documentation #'sly-describe-symbol)

  (sp-with-modes '(sly-mrepl-mode)
    (sp-local-pair "'" "'" :actions nil)
    (sp-local-pair "`" "`" :actions nil))

  ;;
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
    (cond ((or (doom-temp-buffer-p (current-buffer))
               (sly-connected-p)))
          ((executable-find inferior-lisp-program)
           (let ((sly-auto-start 'always))
             (sly-auto-start)
             (add-hook 'kill-buffer-hook #'+common-lisp|cleanup-sly-maybe nil t)))
          ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                    inferior-lisp-program))))
  (add-hook 'sly-mode-hook #'+common-lisp|init-sly)

  (defun +common-lisp*refresh-sly-version (version conn)
    "Update `sly-protocol-version', which will likely be incorrect or nil due to
an issue where `load-file-name' is incorrect. Because Doom's packages are
installed through an external script (bin/doom), `load-file-name' is set to
bin/doom while packages at compile-time (not a runtime though)."
    (unless sly-protocol-version
      (setq sly-protocol-version (sly-version nil (locate-library "sly.el"))))
    (advice-remove #'sly-check-version #'+common-lisp*refresh-sly-version))
  (advice-add #'sly-check-version :before #'+common-lisp*refresh-sly-version)

  (map! :map sly-mode-map
        :localleader
        :n "'" #'sly
        :n "g" #'+common-lisp/navigation/body
        (:prefix "h"
          :n "a" #'sly-apropos
          :n "b" #'sly-who-binds
          :n "d" #'sly-disassemble-symbol
          :n "h" #'sly-describe-symbol
          :n "H" #'sly-hyperspec-lookup
          :n "m" #'sly-who-macroexpands
          :n "p" #'sly-apropos-package
          :n "r" #'sly-who-references
          :n "s" #'sly-who-specializes
          :n "S" #'sly-who-sets
          :n "<" #'sly-who-calls
          :n ">" #'sly-calls-who)
        (:prefix "c"
          :n "c" #'sly-compile-file
          :n "C" #'sly-compile-and-load-file
          :n "f" #'sly-compile-defun
          :n "l" #'sly-load-file
          :n "n" #'sly-remove-notes
          :n "r" #'sly-compile-region)
        (:prefix "e"
          :n "b" #'sly-eval-buffer
          :n "e" #'sly-eval-last-expression
          :n "E" #'sly-eval-print-last-expression
          :n "f" #'sly-eval-defun
          :n "F" #'sly-undefine-function
          :n "r" #'sly-eval-region)
        (:prefix "m"
          :n "e" #'macrostep-expand
          :n "E" #'+common-lisp/macrostep/body)
        (:prefix "r"
          :n "c" #'sly-mrepl-clear-repl
          :n "q" #'sly-quit-lisp
          :n "r" #'sly-restart-inferior-lisp
          :n "s" #'sly-mrepl-sync)
        (:prefix "s"
          :n "b" #'sly-stickers-toggle-break-on-stickers
          :n "c" #'sly-stickers-clear-defun-stickers
          :n "C" #'sly-stickers-clear-buffer-stickers
          :n "f" #'sly-stickers-fetch
          :n "r" #'sly-stickers-replay
          :n "s" #'sly-stickers-dwim)
        (:prefix "t"
          :n "t" #'sly-toggle-trace-fdefinition
          :n "T" #'sly-toggle-fancy-trace
          :n "u" #'sly-untrace-all))

  ;; Since `evil-collection-slime' exists, but not `evil-collection-sly', we
  ;; simply copy it
  (when (featurep! :feature evil +everywhere)
    (add-hook 'sly-mode-hook #'evil-normalize-keymaps)
    (add-hook 'sly-popup-buffer-mode-hook #'evil-normalize-keymaps)
    (unless evil-move-beyond-eol
      (advice-add #'sly-eval-last-expression :around #'+common-lisp*sly-last-sexp)
      (advice-add #'sly-pprint-eval-last-expression :around #'+common-lisp*sly-last-sexp)
      (advice-add #'sly-eval-print-last-expression :around #'+common-lisp*sly-last-sexp)
      (advice-add #'sly-eval-last-expression-in-repl :around #'+common-lisp*sly-last-sexp))
    (set-evil-initial-state!
      '(sly-db-mode sly-inspector-mode sly-popup-buffer-mode sly-xref-mode)
      'normal)
    (evil-define-key 'insert sly-mrepl-mode-map
      [S-return] #'newline-and-indent)
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
