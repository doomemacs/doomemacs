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
    (setq nrepl-hide-special-buffers t
          cider-stacktrace-default-filters '(tooling dup)
          cider-prompt-save-file-on-load nil
          cider-repl-use-clojure-font-lock t)
    (map! :map cider-mode-map
          :localleader
          :n  "'"  #'cider-jack-in
          :n  "\"" #'cider-jack-in-clojurescript
          :n  "B"  #'cider-switch-to-repl-buffer
          :n  "n"  #'cider-repl-set-ns
          :n  "j"  #'cider-find-var
          :n  "h"  #'cider-doc
          :n  "c"  #'cider-repl-clear-buffer
          :n  "p"  #'cider-eval-sexp-at-point)))


(def-package! clj-refactor
  :after clojure-mode)
