;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(after! clojure-mode
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

  (set-repl-handler! 'clojure-mode #'+clojure/repl)
  (set-eval-handler! 'clojure-mode #'cider-eval-region))


(use-package! cider
  ;; NOTE: if you don't have an org directory set (the dir doesn't exist),
  ;; cider jack in won't work.
  :commands cider-jack-in cider-jack-in-clojurescript
  :hook (clojure-mode-local-vars . cider-mode)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (set-lookup-handlers! 'cider-mode
    :definition #'+clojure-cider-lookup-definition
    :documentation #'cider-doc)
  (set-popup-rules!
    '(("^\\*cider-error*" :ignore t)
      ("^\\*cider-repl" :quit nil)
      ("^\\*cider-repl-history" :vslot 2 :ttl nil)))

  (setq nrepl-hide-special-buffers t
        nrepl-log-messages nil
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
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

  ;; Error messages emitted from CIDER is silently funneled into *nrepl-server*
  ;; rather than the *cider-repl* buffer. How silly. We might want to see that
  ;; stuff and who's going to check *nrepl-server* on every startup? I've got a
  ;; better idea: we copy these errors into the *cider-repl* buffer.
  (add-hook! 'cider-connected-hook
    (defun +clojure--cider-dump-nrepl-server-log-h ()
      "Copy contents of *nrepl-server* to beginning of *cider-repl*."
      (save-excursion
        (goto-char (point-min))
        (insert
         (with-current-buffer nrepl-server-buffer
           (buffer-string))))))

  ;; The CIDER welcome message obscures error messages that the above code is
  ;; supposed to be make visible.
  (setq cider-repl-display-help-banner nil)

  (map! (:localleader
          (:map (clojure-mode-map clojurescript-mode-map)
            "'"  #'cider-jack-in-clj
            "\"" #'cider-jack-in-cljs
            "c"  #'cider-connect-clj
            "C"  #'cider-connect-cljs

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
              "e" #'cider-enlighten-mode
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
              "c" #'cider-find-and-clear-repl-output
              "l" #'cider-load-buffer
              "L" #'cider-load-buffer-and-switch-to-repl-buffer)
            (:prefix ("t" . "test")
              "a" #'cider-test-rerun-test
              "l" #'cider-test-run-loaded-tests
              "n" #'cider-test-run-ns-tests
              "p" #'cider-test-run-project-tests
              "r" #'cider-test-rerun-failed-tests
              "s" #'cider-test-run-ns-tests-with-filters
              "t" #'cider-test-run-test)))

        (:when (featurep! :editor evil +everywhere)
          :map cider-repl-mode-map
          :i [S-return] #'cider-repl-newline-and-indent
          :i [M-return] #'cider-repl-return
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


(use-package! clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (set-lookup-handlers! 'clj-refactor-mode
    :references #'cljr-find-usages)
  (map! :map clojure-mode-map
        :localleader
        :desc "refactor" "R" #'hydra-cljr-help-menu/body))


(use-package! flycheck-joker
  :when (featurep! :tools flycheck)
  :after flycheck)
