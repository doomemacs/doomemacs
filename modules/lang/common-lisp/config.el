;;; lang/common-lisp/config.el -*- lexical-binding: t; -*-

;; `lisp-mode' is loaded at startup. In order to lazy load its config we need to
;; pretend it isn't loaded
(defer-feature! lisp-mode)


;;
;; packages

(defvar inferior-lisp-program "sbcl")

(after! lisp-mode
  (set-repl-handler! 'lisp-mode #'sly-mrepl)
  (set-eval-handler! 'lisp-mode #'sly-eval-region)
  (set-lookup-handlers! 'lisp-mode
    :definition #'sly-edit-definition
    :documentation #'sly-describe-symbol)

  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))


(after! sly
  (setq sly-mrepl-history-file-name (concat doom-cache-dir "sly-mrepl-history")
        sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        ;; Change this to `sly-flex-completions' for fuzzy completion
        sly-complete-symbol-function 'sly-simple-completions)

  (set-popup-rules!
    '(("^\\*sly-mrepl"       :vslot 2 :quit nil :ttl nil)
      ("^\\*sly-compilation" :vslot 3 :ttl nil)
      ("^\\*sly-traces"      :vslot 4 :ttl nil)))

  ;; Do not display debugger or inspector buffers in a popup window.
  ;; These buffers are meant to be displayed with sufficient vertical space.
  (set-popup-rule! "^\\*sly-\\(db\\|inspector\\)" :ignore t)

  (sp-with-modes '(sly-mrepl-mode)
    (sp-local-pair "'" "'" :actions nil)
    (sp-local-pair "`" "`" :actions nil))

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

  (map! :localleader
        :map sly-mode-map
        "'" #'sly
        (:prefix "g"
          "b" #'sly-pop-find-definition-stack
          "d" #'sly-edit-definition
          "D" #'sly-edit-definition-other-window
          "n" #'sly-next-note
          "N" #'sly-previous-note
          "s" #'sly-stickers-next-sticker
          "S" #'sly-stickers-prev-sticker)
        (:prefix "h"
          "<" #'sly-who-calls
          ">" #'sly-calls-who
          "~" #'hyperspec-lookup-format
          "#" #'hyperspec-lookup-reader-macro
          "a" #'sly-apropos
          "b" #'sly-who-binds
          "d" #'sly-disassemble-symbol
          "h" #'sly-describe-symbol
          "H" #'sly-hyperspec-lookup
          "m" #'sly-who-macroexpands
          "p" #'sly-apropos-package
          "r" #'sly-who-references
          "s" #'sly-who-specializes
          "S" #'sly-who-sets)
        (:prefix "c"
          "c" #'sly-compile-file
          "C" #'sly-compile-and-load-file
          "f" #'sly-compile-defun
          "l" #'sly-load-file
          "n" #'sly-remove-notes
          "r" #'sly-compile-region)
        (:prefix "e"
          "b" #'sly-eval-buffer
          "e" #'sly-eval-last-expression
          "E" #'sly-eval-print-last-expression
          "f" #'sly-eval-defun
          "F" #'sly-undefine-function
          "r" #'sly-eval-region)
        (:prefix "m"
          "e" #'+common-lisp/macrostep/body
          "E" #'macrostep-expand)
        (:prefix "r"
          "c" #'sly-mrepl-clear-repl
          "q" #'sly-quit-lisp
          "r" #'sly-restart-inferior-lisp
          "s" #'sly-mrepl-sync)
        (:prefix "s"
          "b" #'sly-stickers-toggle-break-on-stickers
          "c" #'sly-stickers-clear-defun-stickers
          "C" #'sly-stickers-clear-buffer-stickers
          "f" #'sly-stickers-fetch
          "r" #'sly-stickers-replay
          "s" #'sly-stickers-dwim)
        (:prefix "t"
          "t" #'sly-toggle-trace-fdefinition
          "T" #'sly-toggle-fancy-trace
          "u" #'sly-untrace-all))

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
      [follow-link] 'mouse-face
      [mouse-2]  'sly-db-default-action/mouse
      [return] 'sly-db-default-action
      (kbd "C-i") 'sly-db-cycle
      (kbd "C-j") 'sly-db-down
      (kbd "C-k") 'sly-db-up
      (kbd "C-m") 'sly-db-default-action
      (kbd "C-S-j") 'sly-db-details-down
      (kbd "C-S-k") 'sly-db-details-up
      "]" 'sly-db-details-down
      "[" 'sly-db-details-up
      "0" 'sly-db-invoke-restart-0
      "1" 'sly-db-invoke-restart-1
      "2" 'sly-db-invoke-restart-2
      "3" 'sly-db-invoke-restart-3
      "4" 'sly-db-invoke-restart-4
      "5" 'sly-db-invoke-restart-5
      "6" 'sly-db-invoke-restart-6
      "7" 'sly-db-invoke-restart-7
      "8" 'sly-db-invoke-restart-8
      "9" 'sly-db-invoke-restart-9
      "a" 'sly-db-abort
      "A" 'sly-db-break-with-system-debugger
      "b" 'sly-db-break-on-return
      "B" 'sly-db-break-with-default-debugger
      "c" 'sly-db-continue
      "C" 'sly-db-inspect-condition
      "d" 'sly-db-pprint-eval-in-frame
      "D" 'sly-db-disassemble
      "e" 'sly-db-eval-in-frame
      "g:" 'sly-interactive-eval
      "g?" 'describe-mode
      "gg" 'sly-db-beginning-of-backtrace
      "gj" 'sly-db-down
      "gk" 'sly-db-up
      "gr" 'sly-db-restart-frame
      "G" 'sly-db-end-of-backtrace
      "i" 'sly-db-inspect-in-frame
      "I" 'sly-db-invoke-restart-by-name
      "n" 'sly-db-next
      "o" 'sly-db-out
      "P" 'sly-db-print-condition
      "q" 'sly-db-quit
      "R" 'sly-db-return-from-frame
      "s" 'sly-db-step
      "S" 'sly-db-show-frame-source
      "t" 'sly-db-toggle-details)
    (evil-define-key 'normal sly-inspector-mode-map
      [backtab] 'sly-inspector-previous-inspectable-object
      [mouse-1] 'sly-inspector-operate-on-click
      [mouse-2] 'sly-inspector-operate-on-click
      [mouse-6] 'sly-inspector-pop
      [mouse-7] 'sly-inspector-next
      [return] 'sly-inspector-operate-on-point
      [(shift tab)] 'sly-inspector-previous-inspectable-object
      (kbd "<M-return>") 'sly-mrepl-copy-part-to-repl
      (kbd "C-i") 'sly-inspector-next-inspectable-object
      (kbd "C-k") 'sly-inspector-pop
      (kbd "C-m") 'sly-inspector-operate-on-point
      "." 'sly-inspector-show-source
      "D" 'sly-inspector-describe-inspectee
      "e" 'sly-inspector-eval
      "gb" 'sly-inspector-pop
      "gj" 'sly-inspector-next
      "gr" 'sly-inspector-reinspect
      "gR" 'sly-inspector-fetch-all
      "gv" 'sly-inspector-toggle-verbose
      "j" 'sly-inspector-next
      "h" 'sly-inspector-history
      "k" 'sly-inspector-previous-inspectable-object
      "K" 'sly-inspector-describe
      "p" 'sly-inspector-pprint
      "q" 'sly-inspector-quit)
    (evil-define-key 'normal sly-mode-map
      (kbd "C-t") 'sly-pop-find-definition-stack)
    (evil-define-key 'normal sly-popup-buffer-mode-map
      (kbd "C-t") 'sly-pop-find-definition-stack
      "q" 'quit-window)
    (evil-define-key 'normal sly-xref-mode-map
      [return] 'sly-goto-xref
      (kbd "S-<return>") 'sly-show-xref
      (kbd "C-j") 'sly-xref-next-line
      (kbd "C-k") 'sly-xref-prev-line
      "]" 'sly-xref-next-line
      "[" 'sly-xref-prev-line
      "gj" 'sly-xref-next-line
      "gk" 'sly-xref-prev-line
      "go" 'sly-show-xref
      "gr" 'sly-recompile-xref
      "gR" 'sly-recompile-all-xrefs
      "r" 'sly-xref-retract)))


(def-package! sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color nil #'eq))
