;;; module-octave.el

(use-package octave
  :mode (("\\.m$" . octave-mode))
  :config
  ;; (setq inferior-octave-program "/usr/local/bin/octave")
  )

;; (use-package ac-octave
;;   :config
;;   (add-hook! octave-mode
;;     (require 'ac-octave)
;;     (setq ac-sources '(ac-source-octave))))

(use-package octave-inf :commands (run-octave))

(provide 'module-octave)
;;; module-octave.el ends here
