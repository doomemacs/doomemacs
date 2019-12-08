;;; lang/scheme/config.el -*- lexical-binding: t; -*-

;;;###package scheme
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)


(use-package! geiser
  :defer t
  :init
  (setq geiser-active-implementations '(guile chicken mit chibi chez))
  (unless (featurep! :lang racket)
    (push 'racket geiser-active-implementations))
  (after! scheme  ; built-in
    (set-repl-handler! 'scheme-mode '+scheme/open-repl)
    (set-eval-handler! 'scheme-mode #'geiser-eval-region)
    (set-lookup-handlers! 'scheme-mode
      :definition #'geiser-edit-symbol-at-point
      :documentation #'geiser-doc-symbol-at-point))
  :config
  (set-popup-rules!
    '(("\\*[Gg]eiser \\(?:[Mm]essages\\|DBG\\|Xref\\)\\*" :quit nil)
      ( "\\* [A-Za-z0-9_-]+ REPL \\*" :quit nil)))
  (map! :localleader
        :map scheme-mode-map
        "'" #'geiser-mode-switch-to-repl
        "s" #'geiser-set-scheme

        (:prefix ("e" . "eval")
          "b" #'geiser-eval-buffer
          "B" #'geiser-eval-buffer-and-go
          "e" #'geiser-eval-definition
          "E" #'geiser-eval-definition-and-go
          "r" #'geiser-eval-region
          "R" #'geiser-eval-region-and-go)

        (:prefix ("h" . "help")
          "d" 'geiser-autodoc)
        ;; TODO add more help keybindings

        (:prefix ("r" . "repl")
          "b" #'geiser-switch-to-repl
          "q" #'geiser-repl-exit
          "r" #'geiser-restart-repl
          "R" #'geiser-reload
          "c" #'geiser-repl-clear-buffer)))
