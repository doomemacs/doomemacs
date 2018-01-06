;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(def-package! clojure-mode
  :mode "\\.clj$"
  :mode ("\\.cljs$" . clojurescript-mode)
  :config
  (map! :map clojure-mode-map
        (:localleader
         :n  "'"      #'cider-jack-in
         :n  "\""     #'cider-jack-in-clojurescript
         :n  "B"      #'cider-switch-to-repl-buffer
         :n  "b"      #'cider-eval-buffer
         :n  "n"      #'cider-repl-set-ns
         :n  "j"      #'cider-find-var
         :n  "d"      #'cider-doc
         :n  "c"      #'cider-repl-clear-buffer
         :n  "p"      #'cider-eval-sexp-at-point
         :n  "r"      #'cider-eval-region)))


(def-package! clj-refactor
  :after clojure-mode
  :config
  ;; setup some extra namespace auto completion for great awesome
  (dolist (mapping '(("re-frame" . "re-frame.core")
                     ("reagent"  . "reagent.core")
                     ("str"      . "clojure.str")))
    (add-to-list 'cljr-magic-require-namespaces mapping t)))


(def-package! cider
  ;; NOTE: if you don't have an org directory set (the dir doesn't exist), cider jack in won't work.
  :commands (cider-jack-in cider-mode cider-jack-in-clojurescript)
  :config
  (setq nrepl-hide-special-buffers t)

  ;; settings for cider repl as a popup (prevent it from being closed on escape, especially.)
  (set! :popup "^\\*cider" nil '((quit) (select)))

  ;; Setup cider for clojurescript / figwheel dev.
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))"))
