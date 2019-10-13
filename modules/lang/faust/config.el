;;; lang/faust/config.el -*- lexical-binding: t; -*-

(use-package! faustine
  :mode ("\\.dsp\\'" . faustine-mode)
  :config

  (set-company-backend! '(faust-mode faustine-mode) '+faust-company-backend)

  (defadvice! +faust--suppress-ac-warnings-a (orig-fn &rest args)
    "Silence obnoxious 'You really should install and use auto-complete' warnings
when starting faust-mode *and* faustine-mode. You really should *not* install
nor use auto-complete."
    :around '(faust-mode faustine-mode)
    (let (ac-modes ac-sources)
      (apply orig-fn args)))

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
