;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(def-package! clojure-mode
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$". clojurescript-mode))
  :commands (clojure-mode clojurescript-mode) ;; this might not be necessary
  :config
  (map! :map clojure-mode-map
        (:localleader
          "'"      #'cider-jack-in
          "\""     #'cider-jack-in-clojurescript
          "r"      #'cider-eval-region)))

(def-package! cider
  :commands (cider-jack-in cider-mode cider-jack-in-clojurescript)
  :config
  (setq nrepl-hide-special-buffers t))
