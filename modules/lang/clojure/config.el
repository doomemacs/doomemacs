;;; lang/clojure/config.el -*- lexical-binding: t; -*-

;; `clojure-mode'
(after! clojure-mode
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

  (def-package! cider
    ;; NOTE: if you don't have an org directory set (the dir doesn't exist),
    ;; cider jack in won't work.
    :commands (cider-jack-in cider-jack-in-clojurescript)
    :hook (clojure-mode . cider-mode)
    :init
    (set-popup-rule! "^\\*cider-repl" :quit nil :select nil)
    (set-repl-handler! 'clojure-mode #'+clojure/repl)
    (set-eval-handler! 'clojure-mode #'cider-eval-region)
    (set-lookup-handlers! 'clojure-mode
      :definition #'cider-browse-ns-find-at-point
      :documentation #'cider-browse-ns-doc-at-point)
    :config
    (setq nrepl-log-messages nil
          nrepl-hide-special-buffers t
          cider-font-lock-dynamically '(macro core function var)
          cider-overlays-use-font-lock t
          cider-prompt-for-symbol nil
          cider-repl-display-help-banner nil
          cider-repl-pop-to-buffer-on-connect t
          cider-repl-use-pretty-printing t
          cider-repl-wrap-history nil
          cider-repl-history-display-duplicates nil
          cider-stacktrace-default-filters '(tooling dup)
          cider-repl-use-clojure-font-lock t)

    ;; TODO: Add mode-local labels when general support is in.
    (map! :map cider-mode-map
          :localleader
          :n  "'"  #'cider-jack-in
          :n  "\"" #'cider-jack-in-clojurescript
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
            :n "n" 'cider-find-ns
            :n "a" 'cider-apropos
            :n "d" 'cider-doc
            :n "g" 'cider-grimoire-web
            :n "j" 'cider-javadoc)
          ;; inspect
          (:prefix "i"
            :n "i" 'cider-inspect
            :n "r" 'cider-inspect-last-result)
          ;; macro
          (:prefix "m"
            :n "e" 'cider-macroexpand-1
            :n "E" 'cider-macroexpand-al)
          ;; namespace
          (:prefix "n"
            :n "n" 'cider-browse-ns
            :n "N" 'cider-browse-ns-all)
          ;; repl
          (:prefix "r"
            :n "n" 'cider-repl-set-ns
            :n "q" 'cider-quit
            :n "r" 'cider-refresh
            :n "R" 'cider-restart
            :n "B" #'cider-switch-to-repl-buffer
            :n "c" #'cider-repl-clear-buffer))
    (when (featurep! :feature evil +everywhere)
      (add-hook 'cider-repl-mode-hook #'evil-normalize-keymaps)))

  (def-package! clj-refactor
    :hook (clojure-mode . clj-refactor-mode)
    :config
    (map! :map clj-refactor-map
          :localleader
          :n "r" #'hydra-cljr-help-menu/body))

  (def-package! flycheck-joker
    :when (featurep! :feature syntax-checker)
    :after flycheck))
