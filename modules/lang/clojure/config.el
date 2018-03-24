;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(def-package! clojure-mode
  :mode "\\.clj$"
  :mode "\\.edn$"
  :mode "\\(?:build\\|profile\\)\\.boot$"
  :mode ("\\.cljs$" . clojurescript-mode)
  :mode ("\\.cljc$" . clojurec-mode)
  :config
  (add-hook 'clojure-mode #'rainbow-delimiters-mode))


(def-package! clj-refactor
  :after clojure-mode
  :config
  ;; setup some extra namespace auto completion for great awesome
  (nconc cljr-magic-require-namespaces
         '(("re-frame" . "re-frame.core")
           ("reagent"  . "reagent.core")
           ("str"      . "clojure.string"))))


(def-package! cider
  ;; NOTE: if you don't have an org directory set (the dir doesn't exist), cider
  ;; jack in won't work.
  :commands (cider-jack-in cider-jack-in-clojurescript)
  :hook (clojure-mode . cider-mode)
  :config
  (setq nrepl-hide-special-buffers t
        cider-stacktrace-default-filters '(tooling dup)
        cider-prompt-save-file-on-load nil
        cider-repl-use-clojure-font-lock t
        ;; Setup cider for clojurescript / figwheel dev.
        cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")

  (set! :popup "^\\*cider-repl" nil '((quit) (select)))
  (set! :repl 'clojure-mode #'+clojure/repl)
  (set! :eval 'clojure-mode #'cider-eval-region)
  (set! :lookup 'clojure-mode
    :definition #'cider-browse-ns-find-at-point
    :documentation #'cider-browse-ns-doc-at-point)

  (map! :map cider-mode-map
        :localleader
        :n  "'"  #'cider-jack-in
        :n  "\"" #'cider-jack-in-clojurescript
        :n  "B"  #'cider-switch-to-repl-buffer
        :n  "b"  #'cider-eval-buffer
        :n  "n"  #'cider-repl-set-ns
        :n  "j"  #'cider-find-var
        :n  "d"  #'cider-doc
        :n  "c"  #'cider-repl-clear-buffer
        :n  "p"  #'cider-eval-sexp-at-point
        :n  "r"  #'cider-eval-region))
