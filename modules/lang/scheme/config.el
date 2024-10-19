;;; lang/scheme/config.el -*- lexical-binding: t; -*-

(use-package! scheme
  :interpreter ("scsh" . scheme-mode)
  :hook (scheme-mode . rainbow-delimiters-mode)
  :config
  (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes '(scheme-mode))
  (advice-add #'scheme-indent-function :override #'+scheme-indent-function-a))


(use-package! geiser
  :defer t
  :init
  (setq geiser-autodoc-identifier-format "%s → %s"
        geiser-repl-per-project-p t
        geiser-repl-current-project-function #'doom-project-root
        geiser-repl-history-filename (concat doom-cache-dir "geiser-history"))

  (after! scheme  ; built-in
    (set-repl-handler! 'scheme-mode #'+scheme/open-repl
      :persist t
      :send-region #'geiser-eval-region
      :send-buffer #'geiser-eval-buffer)
    (set-eval-handler! 'scheme-mode #'geiser-eval-region)
    (set-lookup-handlers! '(scheme-mode geiser-repl-mode)
      :definition #'geiser-edit-symbol-at-point
      :documentation #'geiser-doc-symbol-at-point))
  :config
  (set-popup-rules!
    '(("^\\*[gG]eiser \\(dbg\\|xref\\|messages\\)\\*$" :slot 1 :vslot -1)
      ("^\\*Geiser documentation\\*$" :slot 2 :vslot 2 :select t :size 0.35)
      ("^\\*Geiser .+ REPL" :size 0.3 :quit nil :ttl nil)))
  
  (map! :localleader
        (:map (scheme-mode-map geiser-repl-mode-map)
         :desc "Toggle REPL"                  "'"  #'geiser-repl-switch
         :desc "Connect to external Scheme"   "\"" #'geiser-connect
         :desc "Toggle type of brackets"      "["  #'geiser-squarify
         :desc "Insert lambda"                "\\" #'geiser-insert-lambda
         :desc "Set Scheme implementation"    "s"  #'geiser-set-scheme
         :desc "Reload Geiser buffers+REPLs"  "R" #'geiser-reload
         (:prefix ("h" . "help")
          :desc "Show callers of <point>"     "<" #'geiser-xref-callers
          :desc "Show callees of <point>"     ">" #'geiser-xref-callees
          :desc "Toggle autodoc mode"         "a" #'geiser-autodoc-mode
          :desc "Show autodoc of <point>"     "s" #'geiser-autodoc-show
          :desc "Search manual for <point>"   "m" #'geiser-doc-look-up-manual
          :desc "Show docstring of <point>"   "." #'geiser-doc-symbol-at-point)
         (:prefix ("r" . "repl")
          :desc "Load file into REPL"         "f" #'geiser-load-file
          :desc "Restart REPL"                "r" #'geiser-restart-repl))
        (:map scheme-mode-map
         (:prefix ("e" . "eval")
          :desc "Eval buffer"                 "b" #'geiser-eval-buffer
          :desc "Eval buffer and go to REPL"  "B" #'geiser-eval-buffer-and-go
          :desc "Eval last sexp"              "e" #'geiser-eval-last-sexp
          :desc "Eval definition"             "d" #'geiser-eval-definition
          :desc "Eval defn. and go to REPL"   "D" #'geiser-eval-definition-and-go
          :desc "Eval region"                 "r" #'geiser-eval-region
          :desc "Eval region and go to REPL"  "R" #'geiser-eval-region-and-go)
         (:prefix ("r" . "repl")
          :desc "Load current buffer in REPL" "b" #'geiser-load-current-buffer))
        (:map geiser-repl-mode-map
         :desc "Clear REPL buffer"            "c" #'geiser-repl-clear-buffer
         :desc "Quit REPL"                    "q" #'geiser-repl-exit)))


(use-package! macrostep-geiser
  :hook (geiser-mode . macrostep-geiser-setup)
  :hook (geiser-repl-mode . macrostep-geiser-setup)
  :init
  (map! :after geiser
        :localleader
        :map (scheme-mode-map geiser-repl-mode-map)
        :desc "Expand macro by one step" "m" #'macrostep-expand
        :desc "Recursively expand macro" "M" #'macrostep-geiser-expand-all))


(use-package! flycheck-guile
  :when (modulep! +guile)
  :when (modulep! :checkers syntax -flymake)
  :after geiser)

;; Add Guix channels to Guile load path
(when (and (modulep! +guile) (executable-find "guix"))
  (after! geiser-guile
    (add-to-list 'geiser-guile-load-path
                 (expand-file-name "~/.config/guix/current/share/guile/site/3.0"))))
