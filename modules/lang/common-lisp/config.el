;;; lang/common-lisp/config.el -*- lexical-binding: t; -*-

;; `lisp-mode' is loaded at startup. In order to lazy load its config we need to
;; pretend it isn't loaded
(defer-feature! lisp-mode)


;;
;; packages

;;;###package lisp-mode
(defvar inferior-lisp-program "sbcl")
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)


(use-package! sly
  :defer t
  :init
  (after! lisp-mode
    (set-repl-handler! 'lisp-mode #'sly-mrepl)
    (set-eval-handler! 'lisp-mode #'sly-eval-region)
    (set-lookup-handlers! 'lisp-mode
      :definition #'sly-edit-definition
      :documentation #'sly-describe-symbol))

  ;; HACK Ensures that sly's contrib modules are loaded as soon as possible, but
  ;;      also as late as possible, so users have an opportunity to override
  ;;      `sly-contrib' in an `after!' block.
  (add-hook! 'doom-after-init-modules-hook
    (after! sly (sly-setup)))

  :config
  (setq sly-mrepl-history-file-name (concat doom-cache-dir "sly-mrepl-history")
        sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        ;; Doom defaults to non-fuzzy search, because it is faster and more
        ;; precise (but requires more keystrokes). Change this to
        ;; `sly-flex-completions' for fuzzy completion
        sly-complete-symbol-function 'sly-simple-completions)

  (set-popup-rules!
    '(("^\\*sly-mrepl"       :vslot 2 :size 0.3 :quit nil :ttl nil)
      ("^\\*sly-compilation" :vslot 3 :ttl nil)
      ("^\\*sly-traces"      :vslot 4 :ttl nil)
      ("^\\*sly-description" :vslot 5 :size 0.3 :ttl 0)
      ;; Do not display debugger or inspector buffers in a popup window. These
      ;; buffers are meant to be displayed with sufficient vertical space.
      ("^\\*sly-\\(?:db\\|inspector\\)" :ignore t)))

  (defun +common-lisp--cleanup-sly-maybe-h ()
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

  (add-hook! 'sly-mode-hook
    (defun +common-lisp-init-sly-h ()
      "Attempt to auto-start sly when opening a lisp buffer."
      (cond ((or (doom-temp-buffer-p (current-buffer))
                 (sly-connected-p)))
            ((executable-find inferior-lisp-program)
             (let ((sly-auto-start 'always))
               (sly-auto-start)
               (add-hook 'kill-buffer-hook #'+common-lisp--cleanup-sly-maybe-h nil t)))
            ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                      inferior-lisp-program)))))

  (map! :localleader
        :map lisp-mode-map
        :desc "Sly"          "'" #'sly
        :desc "Sly (ask)"    ";" (λ!! #'sly '-)
        :desc "Expand macro" "m" #'macrostep-expand
        (:prefix ("c" . "compile")
          :desc "Compile file"          "c" #'sly-compile-file
          :desc "Compile/load file"     "C" #'sly-compile-and-load-file
          :desc "Compile toplevel form" "f" #'sly-compile-defun
          :desc "Load file"             "l" #'sly-load-file
          :desc "Remove notes"          "n" #'sly-remove-notes
          :desc "Compile region"        "r" #'sly-compile-region)
        (:prefix ("e" . "evaluate")
          :desc "Evaulate buffer"     "b" #'sly-eval-buffer
          :desc "Evaluate last"       "e" #'sly-eval-last-expression
          :desc "Evaluate/print last" "E" #'sly-eval-print-last-expression
          :desc "Evaluate defun"      "f" #'sly-eval-defun
          :desc "Undefine function"   "F" #'sly-undefine-function
          :desc "Evaluate region"     "r" #'sly-eval-region)
        (:prefix ("g" . "goto")
          :desc "Go back"              "b" #'sly-pop-find-definition-stack
          :desc "Go to"                "d" #'sly-edit-definition
          :desc "Go to (other window)" "D" #'sly-edit-definition-other-window
          :desc "Next note"            "n" #'sly-next-note
          :desc "Previous note"        "N" #'sly-previous-note
          :desc "Next sticker"         "s" #'sly-stickers-next-sticker
          :desc "Previous sticker"     "S" #'sly-stickers-prev-sticker)
        (:prefix ("h" . "help")
          :desc "Who calls"               "<" #'sly-who-calls
          :desc "Calls who"               ">" #'sly-calls-who
          :desc "Lookup format directive" "~" #'hyperspec-lookup-format
          :desc "Lookup reader macro"     "#" #'hyperspec-lookup-reader-macro
          :desc "Apropos"                 "a" #'sly-apropos
          :desc "Who binds"               "b" #'sly-who-binds
          :desc "Disassemble symbol"      "d" #'sly-disassemble-symbol
          :desc "Describe symbol"         "h" #'sly-describe-symbol
          :desc "HyperSpec lookup"        "H" #'sly-hyperspec-lookup
          :desc "Who macro-expands"       "m" #'sly-who-macroexpands
          :desc "Apropos package"         "p" #'sly-apropos-package
          :desc "Who references"          "r" #'sly-who-references
          :desc "Who specializes"         "s" #'sly-who-specializes
          :desc "Who sets"                "S" #'sly-who-sets)
        (:prefix ("r" . "repl")
          :desc "Clear REPL"         "c" #'sly-mrepl-clear-repl
          :desc "Quit connection"    "q" #'sly-quit-lisp
          :desc "Restart connection" "r" #'sly-restart-inferior-lisp
          :desc "Sync REPL"          "s" #'sly-mrepl-sync)
        (:prefix ("s" . "stickers")
          :desc "Toggle breaking stickers" "b" #'sly-stickers-toggle-break-on-stickers
          :desc "Clear defun stickers"     "c" #'sly-stickers-clear-defun-stickers
          :desc "Clear buffer stickers"    "C" #'sly-stickers-clear-buffer-stickers
          :desc "Fetch stickers"           "f" #'sly-stickers-fetch
          :desc "Replay stickers"          "r" #'sly-stickers-replay
          :desc "Add/remove sticker"       "s" #'sly-stickers-dwim)
        (:prefix ("t" . "trace")
          :desc "Toggle"         "t" #'sly-toggle-trace-fdefinition
          :desc "Toggle (fancy)" "T" #'sly-toggle-fancy-trace
          :desc "Untrace all"    "u" #'sly-untrace-all))

  (when (featurep! :editor evil +everywhere)
    (add-hook 'sly-mode-hook #'evil-normalize-keymaps)))


(use-package! sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))
