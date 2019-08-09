;;; lang/clojure/config.el -*- lexical-binding: t; -*-

;;;###package clojure-mode
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)


(def-package! cider
  ;; NOTE: if you don't have an org directory set (the dir doesn't exist),
  ;; cider jack in won't work.
  :commands (cider-jack-in cider-jack-in-clojurescript)
  :hook (clojure-mode-local-vars . cider-mode)
  :init
  (set-repl-handler! 'clojure-mode #'+clojure/repl)
  (set-eval-handler! 'clojure-mode #'cider-eval-region)
  (set-lookup-handlers! 'cider-mode
    :definition #'+clojure-cider-lookup-definition
    :documentation #'cider-doc)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  :config
  (set-popup-rules!
    '(("^\\*cider-error*" :ignore t)
      ("^\\*cider-repl" :quit nil)
      ("^\\*cider-repl-history" :vslot 2 :ttl nil)))

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

  (map! (:localleader
          (:map clojure-mode-map
            "'"  #'cider-jack-in
            "\"" #'cider-jack-in-clojurescript

            (:prefix ("e" . "eval")
              "d" #'cider-eval-defun-at-point
              "D" #'cider-insert-defun-in-repl
              "e" #'cider-eval-last-sexp
              "E" #'cider-insert-last-sexp-in-repl
              "r" #'cider-eval-region
              "R" #'cider-insert-region-in-repl
              "u" #'cider-undef)
            (:prefix ("g" . "go/jump")
              "b" #'cider-pop-back
              "g" #'cider-find-var
              "n" #'cider-find-ns)
            (:prefix ("h" . "help")
              "n" #'cider-find-ns
              "a" #'cider-apropos
              "d" #'cider-doc
              "g" #'cider-grimoire-web
              "j" #'cider-javadoc)
            (:prefix ("i" . "inspect")
              "i" #'cider-inspect
              "r" #'cider-inspect-last-result)
            (:prefix ("m" . "macro")
              "e" #'cider-macroexpand-1
              "E" #'cider-macroexpand-al)
            (:prefix ("n" . "namespace")
              "n" #'cider-browse-ns
              "N" #'cider-browse-ns-all)
            (:prefix ("r" . "repl")
              "n" #'cider-repl-set-ns
              "q" #'cider-quit
              "r" #'cider-refresh
              "R" #'cider-restart
              "b" #'cider-switch-to-repl-buffer
              "B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
              "c" #'cider-find-and-clear-repl-output)))

        (:when (featurep! :editor evil +everywhere)
          :map cider-repl-mode-map
          :i [S-return] #'cider-repl-newline-and-indent
          (:localleader
            ("n" #'cider-repl-set-ns
             "q" #'cider-quit
             "r" #'cider-ns-refresh
             "R" #'cider-restart
             "c" #'cider-repl-clear-buffer))
          :map cider-repl-history-mode-map
          :i [return]  #'cider-repl-history-insert-and-quit
          :i "q"  #'cider-repl-history-quit
          :i "l"  #'cider-repl-history-occur
          :i "s"  #'cider-repl-history-search-forward
          :i "r"  #'cider-repl-history-search-backward
          :i "U"  #'cider-repl-history-undo-other-window)))


(def-package! clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :init
  (set-lookup-handlers! 'clj-refactor-mode
    :references #'cljr-find-usages)
  :config
  (map! :map clojure-mode-map
        :localleader
        :desc "refactor" "R" #'hydra-cljr-help-menu/body))


(def-package! flycheck-joker
  :when (featurep! :tools flycheck)
  :after flycheck)
