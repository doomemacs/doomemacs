;;; lang/octave/config.el

;; built-in
(def-package! octave
  :mode (("\\.m$" . octave-mode))
  :commands (octave-mode run-octave)
  :config (set! :repl 'octave-mode #'run-octave))

;; (use-package ac-octave
;;   :config
;;   (add-hook! octave-mode
;;     (require 'ac-octave)
;;     (setq ac-sources '(ac-source-octave))))

