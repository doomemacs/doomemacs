;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(def-package! clojure-mode
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$". clojurescript-mode))
  :commands (clojure-mode clojurescript-mode)
  :config


  (def-package! clj-refactor  ;; prob a better place to do this...using hooks?
    :demand t
    :config
    ;; setup some extra namespace auto completion for great awesome
    (dolist (mapping '(("re-frame" . "re-frame.core")
                       ("reagent"  . "reagent.core")
                       ("str"      . "clojure.str")))
      (add-to-list 'cljr-magic-require-namespaces mapping t)))

  (map! :map clojure-mode-map
        (:localleader
         :n  "'"      #'cider-jack-in
         :n  "\""     #'cider-jack-in-clojurescript
         :n  "b"      #'cider-switch-to-repl-buffer
         :n  "n"      #'cider-repl-set-ns
         :n  "j"      #'cider-find-var
         :n  "d"      #'cider-doc
         :n  "c"      #'cider-repl-clear-buffer
         :n  "p"      #'cider-eval-sexp-at-point
         :n  "r"      #'cider-eval-region)))


(def-package! cider
  ;; BUG: if you don't have an org directory set (the dir doesn't exist), cider jack in won't work.
  :commands (cider-jack-in cider-mode cider-jack-in-clojurescript)
  :config
  (setq nrepl-hide-special-buffers t)

  ;; TODO: figure out how to set cider to the right window rather than shackle window (https://github.com/hlissner/doom-emacs/issues/171)
  ;; (set! :popup "^\\*cider-.*" :align 'left :noesc t) ;; no luck

  ;; Setup cider for clojurescript / figwheel dev.
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))"))
