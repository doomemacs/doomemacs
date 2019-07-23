;;; lang/faust/config.el -*- lexical-binding: t; -*-

(def-package! faustine
  :diminish faustine-mode
  :defer t
  :mode ("\\.dsp\\'" . faustine-mode)
  :config

  (map! :localleader
        :map faustine-mode-map
        "b" #'faustine-build
        "c" #'faustine-syntax-check
        "d" #'faustine-diagram
        "h" #'faustine-online-doc
        "RET" #'faustine-mdoc
        "o" #'faustine-toggle-output-buffer
        "s" #'faustine-source-code
        "r" #'faustine-run
        "S-b" #'faustine-build-all
        "S-d" #'faustine-diagram-all
        ))
