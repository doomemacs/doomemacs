;;; lang/faust/config.el -*- lexical-binding: t; -*-

(use-package! faustine
  :mode ("\\.dsp\\'" . faustine-mode)
  :config
  (set-company-backend! '(faust-mode faustine-mode) '(company-dabbrev-code +faust-company-backend company-yasnippet))

  ;; HACK Both `faust-mode' and `faustine-mode' are hardcoded to use
  ;; auto-complete. This silences the obnoxious 'You really should install and
  ;; use auto-complete' warnings when starting them.
  (defvar ac-modes nil)
  (defvar ac-sources nil)

  (map! :localleader
        :map faustine-mode-map
        "RET" #'faustine-mdoc
        "b" #'faustine-build
        "B" #'faustine-build-all
        "c" #'faustine-syntax-check
        "d" #'faustine-diagram
        "D" #'faustine-diagram-all
        "h" #'faustine-online-doc
        "o" #'faustine-toggle-output-buffer
        "s" #'faustine-source-code
        "r" #'faustine-run))
