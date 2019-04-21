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
      ("^\\*sly-traces"      :vslot 4 :ttl nil)
      ;; Do not display debugger or inspector buffers in a popup window. These
      ;; buffers are meant to be displayed with sufficient vertical space.
      ("^\\*sly-\\(?:db\\|inspector\\)" :ignore t)))

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
        :map lisp-mode-map
        :desc "Sly" "'" #'sly
        :desc "Sly (ask)" ";" (λ! () (let ((current-prefix-arg '-)) (sly nil nil t)))
        (:prefix ("g" . "Go")
          :desc "Go back" "b" #'sly-pop-find-definition-stack
          :desc "Go to" "d" #'sly-edit-definition
          :desc "Go to (other window)" "D" #'sly-edit-definition-other-window
          :desc "Next note" "n" #'sly-next-note
          :desc "Previous note" "N" #'sly-previous-note
          :desc "Next sticker" "s" #'sly-stickers-next-sticker
          :desc "Previous sticker" "S" #'sly-stickers-prev-sticker)
        (:prefix ("h" . "Help")
          :desc "Who calls" "<" #'sly-who-calls
          :desc "Calls who" ">" #'sly-calls-who
          :desc "Lookup format directive" "~" #'hyperspec-lookup-format
          :desc "Lookup reader macro" "#" #'hyperspec-lookup-reader-macro
          :desc "Apropos" "a" #'sly-apropos
          :desc "Who binds" "b" #'sly-who-binds
          :desc "Disassemble symbol" "d" #'sly-disassemble-symbol
          :desc "Describe symbol" "h" #'sly-describe-symbol
          :desc "HyperSpec lookup" "H" #'sly-hyperspec-lookup
          :desc "Who macro-expands" "m" #'sly-who-macroexpands
          :desc "Apropos package" "p" #'sly-apropos-package
          :desc "Who references" "r" #'sly-who-references
          :desc "Who specializes" "s" #'sly-who-specializes
          :desc "Who sets" "S" #'sly-who-sets)
        (:prefix ("c" . "Compile")
          :desc "Compile file" "c" #'sly-compile-file
          :desc "Compile/load file" "C" #'sly-compile-and-load-file
          :desc "Compile toplevel form" "f" #'sly-compile-defun
          :desc "Load file" "l" #'sly-load-file
          :desc "Remove notes" "n" #'sly-remove-notes
          :desc "Compile region" "r" #'sly-compile-region)
        (:prefix ("e" . "Evaluate")
          :desc "Evaulate buffer" "b" #'sly-eval-buffer
          :desc "Evaluate last" "e" #'sly-eval-last-expression
          :desc "Evaluate/print last" "E" #'sly-eval-print-last-expression
          :desc "Evaluate defun" "f" #'sly-eval-defun
          :desc "Undefine function" "F" #'sly-undefine-function
          :desc "Evaluate region" "r" #'sly-eval-region)
        (:prefix ("m" . "Macro")
          :desc "Macrostep" "e" #'macrostep-expand)
        (:prefix ("r" . "REPL")
          :desc "Clear REPL" "c" #'sly-mrepl-clear-repl
          :desc "Quit connection" "q" #'sly-quit-lisp
          :desc "Restart connection" "r" #'sly-restart-inferior-lisp
          :desc "Sync REPL" "s" #'sly-mrepl-sync)
        (:prefix ("s" . "Stickers")
          :desc "Toggle breaking stickers" "b" #'sly-stickers-toggle-break-on-stickers
          :desc "Clear defun stickers" "c" #'sly-stickers-clear-defun-stickers
          :desc "Clear buffer stickers" "C" #'sly-stickers-clear-buffer-stickers
          :desc "Fetch stickers" "f" #'sly-stickers-fetch
          :desc "Replay stickers" "r" #'sly-stickers-replay
          :desc "Add/remove sticker" "s" #'sly-stickers-dwim)
        (:prefix ("t" . "Trace")
          :desc "Toggle" "t" #'sly-toggle-trace-fdefinition
          :desc "Toggle (fancy)" "T" #'sly-toggle-fancy-trace
          :desc "Untrace all" "u" #'sly-untrace-all))

  (when (featurep! :editor evil +everywhere)
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
      [S-return] #'newline-and-indent
      [backspace] #'sp-backward-delete-char
      [up] (λ! () (evil-goto-line) (comint-previous-input 1))
      [down] (λ! () (evil-goto-line) (comint-next-input 1)))
    (evil-define-key 'normal sly-parent-map
      (kbd "C-t") #'sly-pop-find-definition-stack)
    (evil-define-key 'normal sly-popup-buffer-mode-map
      (kbd "C-t") 'sly-pop-find-definition-stack
      "q" 'quit-window)
    (evil-define-key 'normal sly-db-mode-map
      [follow-link] 'mouse-face
      [mouse-2]  'sly-db-default-action/mouse
      [remap quit-window] 'sly-db-quit
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
      [backtab] 'backward-button
      [return] 'push-button
      [(shift tab)] 'backward-button
      (kbd "<M-return>") 'sly-mrepl-copy-part-to-repl
      (kbd "C-i") 'next-button
      (kbd "C-m") 'push-button
      "e" 'sly-inspector-eval
      "gb" 'sly-inspector-pop
      "gj" 'sly-inspector-next
      "gr" 'sly-inspector-reinspect
      "gR" 'sly-inspector-fetch-all
      "gv" 'sly-inspector-toggle-verbose
      "h" 'sly-inspector-history
      "k" 'backward-button
      "K" 'sly-inspector-describe-inspectee
      "p" 'sly-button-pretty-print
      "q" 'sly-inspector-quit)
    (evil-define-key 'normal sly-mode-map
      (kbd "C-t") 'sly-pop-find-definition-stack)
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
      "q" 'quit-window
      "r" 'sly-xref-retract)))

(def-package! sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color nil #'eq))
