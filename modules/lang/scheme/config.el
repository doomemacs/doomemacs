;;; lang/scheme/config.el -*- lexical-binding: t; -*-

(use-package! scheme
  :hook (scheme-mode . rainbow-delimiters-mode)
  :config (advice-add #'scheme-indent-function :override #'+scheme-scheme-indent-function-a))


(use-package! geiser
  :defer t
  :init
  (setq geiser-active-implementations '(guile chicken mit chibi chez)
        geiser-autodoc-identifier-format "%s → %s"
        geiser-repl-current-project-function 'doom-project-root)
  (if (featurep! :lang racket)
      (setq auto-mode-alist
            (remove '("\\.rkt\\'" . scheme-mode) auto-mode-alist))
    (push 'racket geiser-active-implementations))
  (after! scheme                        ; built-in
    (set-repl-handler! 'scheme-mode #'+scheme/open-repl)
    (set-eval-handler! 'scheme-mode #'geiser-eval-region)
    (set-lookup-handlers! '(scheme-mode geiser-repl-mode)
      :definition #'geiser-edit-symbol-at-point
      :documentation #'geiser-doc-symbol-at-point))
  :config
  (set-popup-rules!
    '(("^\\*geiser messages\\*$" :slot 1 :vslot -1)
      ("^\\*Geiser dbg\\*$"      :slot 1 :vslot -1)
      ("^\\*Geiser xref\\*$"     :slot 1 :vslot -1)
      ("^\\*Geiser documentation\\*$" :slot 2 :vslot 2 :select t :size 0.35)
      ("^\\* [A-Za-z0-9_-]+ REPL \\*" :size 0.3 :quit nil :ttl nil)))
  (map! :localleader
        :map scheme-mode-map
        "'"  #'geiser-mode-switch-to-repl
        "\"" #'geiser-connect
        "["  #'geiser-squarify
        "\\" #'geiser-insert-lambda
        "s"  #'geiser-set-scheme
        (:prefix ("e" . "eval")
          "b" #'geiser-eval-buffer
          "B" #'geiser-eval-buffer-and-go
          "e" #'geiser-eval-last-sexp
          "d" #'geiser-eval-definition
          "D" #'geiser-eval-definition-and-go
          "r" #'geiser-eval-region
          "R" #'geiser-eval-region-and-go)
        (:prefix ("h" . "help")
          "d" #'geiser-autodoc
          "<" #'geiser-xref-callers
          ">" #'geiser-xref-callees
          "i" #'geiser-doc-look-up-manual)
        (:prefix ("m" . "macro")
          "r" #'geiser-expand-region
          "d" #'geiser-expand-definition
          "e" #'geiser-expand-last-sexp)
        (:prefix ("r" . "repl")
          "b" #'geiser-switch-to-repl
          "q" #'geiser-repl-exit
          "l" #'geiser-load-current-buffer
          "r" #'geiser-restart-repl
          "R" #'geiser-reload
          "c" #'geiser-repl-clear-buffer)))

(use-package! flycheck-guile
  :when (featurep! :checkers syntax)
  :after geiser)
