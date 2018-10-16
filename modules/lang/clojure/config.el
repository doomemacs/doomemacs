;;; lang/clojure/config.el -*- lexical-binding: t; -*-

;; `clojure-mode'
(after! clojure-mode
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

  (set-popup-rules!
    '(("^\\*cider-error*" :ignore t)
      ("^\\*cider-repl" :quit nil :select nil)
      ("^\\*cider-repl-history" :vslot 2 :ttl nil)))
 
  (def-package! cider
    ;; NOTE: if you don't have an org directory set (the dir doesn't exist),
    ;; cider jack in won't work.
    :commands (cider-jack-in cider-jack-in-clojurescript)
    :hook (clojure-mode . cider-mode)
    :init
    (set-repl-handler! 'clojure-mode #'+clojure/repl)
    (set-eval-handler! 'clojure-mode #'cider-eval-region)
    (set-lookup-handlers! 'clojure-mode
      :definition #'cider-browse-ns-find-at-point
      :documentation #'cider-browse-ns-doc-at-point)
    (add-hook 'cider-mode-hook #'eldoc-mode)
    :config
    (setq nrepl-hide-special-buffers t
          nrepl-log-messages nil
          cider-font-lock-dynamically '(macro core function var)
          cider-overlays-use-font-lock t
          cider-prompt-for-symbol nil
          cider-repl-display-help-banner nil
          cider-repl-history-display-duplicates nil
          cider-repl-history-display-style 'one-line
          cider-repl-history-file (concat doom-cache-dir "cider-repl-history")
          cider-repl-history-highlight-current-entry t
          cider-repl-history-quit-action 'delete-and-restore
          cider-repl-history-highlight-inserted-item t
          cider-repl-history-size 1000
          cider-repl-pop-to-buffer-on-connect 'display-only
          cider-repl-result-prefix ";; => "
          cider-repl-print-length 100
          cider-repl-use-clojure-font-lock t
          cider-repl-use-pretty-printing t
          cider-repl-wrap-history nil
          cider-stacktrace-default-filters '(tooling dup))

    ;; TODO: Add mode-local labels when general support is in.
    (map! :localleader
          (:map clojure-mode-map
            :n  "'"  #'cider-jack-in
            :n  "\"" #'cider-jack-in-clojurescript)
          (:map cider-mode-map
            ;; eval
            (:prefix "e"
              :n "d" #'cider-eval-defun-at-point
              :n "D" #'cider-insert-defun-in-repl
              :n "e" #'cider-eval-last-sexp
              :n "E" #'cider-insert-last-sexp-in-repl
              :n "r" #'cider-eval-region
              :n "R" #'cider-insert-region-in-repl
              :n "u" #'cider-undef)
            ;; go/jump
            (:prefix "g"
              :n "b" #'cider-pop-back
              :n "g" #'cider-find-var
              :n "n" #'cider-find-ns)
            ;; help
            (:prefix "h"
              :n "n" #'cider-find-ns
              :n "a" #'cider-apropos
              :n "d" #'cider-doc
              :n "g" #'cider-grimoire-web
              :n "j" #'cider-javadoc)
            ;; inspect
            (:prefix "i"
              :n "i" #'cider-inspect
              :n "r" #'cider-inspect-last-result)
            ;; macro
            (:prefix "m"
              :n "e" #'cider-macroexpand-1
              :n "E" #'cider-macroexpand-al)
            ;; namespace
            (:prefix "n"
              :n "n" #'cider-browse-ns
              :n "N" #'cider-browse-ns-all)
            ;; repl
            (:prefix "r"
              :n "n" #'cider-repl-set-ns
              :n "q" #'cider-quit
              :n "r" #'cider-refresh
              :n "R" #'cider-restart
              :n "b" #'cider-switch-to-repl-buffer
              :n "B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
              :n "c" #'cider-repl-clear-buffer)))

    (when (featurep! :feature evil +everywhere)
      (evil-define-key 'insert cider-repl-mode-map
        [S-return] #'cider-repl-newline-and-indent)
      (evil-define-key 'normal cider-repl-history-mode-map
        "q" 'cider-repl-history-quit
        [return] 'cider-repl-history-insert-and-quit
        "l" 'cider-repl-history-occur
        "s" 'cider-repl-history-search-forward
        "r" 'cider-repl-history-search-backward
        "U" 'cider-repl-history-undo-other-window)))

  (def-package! clj-refactor
    :hook (clojure-mode . clj-refactor-mode)
    :config
    (map! :map clj-refactor-map
          :localleader
          :n "R" #'hydra-cljr-help-menu/body))

  (def-package! flycheck-joker
    :when (featurep! :feature syntax-checker)
    :after flycheck))
