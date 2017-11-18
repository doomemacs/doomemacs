;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(def-package! clojure-mode
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$". clojurescript-mode))
  :commands (clojure-mode clojurescript-mode) ;; this might not be necessary?
  :config
  (def-package! clj-refactor :demand t) ;; prob a better way to do this

  (map! :map clojure-mode-map
        (:localleader
         :n  "'"      #'cider-jack-in
         :n  "\""     #'cider-jack-in-clojurescript
         :n  "b"      #'cider-switch-to-repl-buffer
         :n  "n"      #'cider-repl-set-ns
         :n  "j"      #'cider-find-var
         :n  "d"      #'cider-doc
         :n  "c"      #'cider-repl-clear-buffer
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
