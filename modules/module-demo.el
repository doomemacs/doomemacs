;;; module-demo.el --- for collaboration and demonstrations

;; This library offers:
;;   + impatient-mode: for broadcasting my emacs session
;;   + big-mode: for enlarged text while screencasting
;;   + integration with reveal.js for presentations

;; Big-mode settings
(defconst big-mode-font (font-spec :family "Inconsolata" :size 16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package impatient-mode
  :defer t
  :commands httpd-start)

(add-hook! org-load
  (use-package ox-reveal
    :config
    (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.2.0/js/reveal.min.js")))

;;;

(defvar big-mode-font narf-default-font)

(define-minor-mode big-mode
  :init-value nil
  :lighter " BIG"
  :global t
  (narf/load-font (if big-mode big-mode-font narf-default-font)))

(provide 'module-demo)
;;; module-demo.el ends here
