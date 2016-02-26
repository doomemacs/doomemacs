;;; module-demo.el --- for collaboration and demonstrations

;; This library offers:
;;   + impatient-mode: for broadcasting my emacs session
;;   + big-mode: for enlarged text while screencasting
;;   + integration with reveal.js for presentations

;; Big-mode settings
(defconst big-mode-font (font-spec :family "Hack" :size 16))
(defconst big-mode-line-spacing 0)
(defconst big-mode-modeline-height 26)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package impatient-mode
  :defer t
  :commands httpd-start)

(add-hook! org-load
  (use-package ox-reveal
    :config
    (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.2.0/js/reveal.min.js")))

;;;

(defvar big-mode--line-spacing line-spacing)
(defvar big-mode--powerline-height powerline-height)

(define-minor-mode big-mode
  :init-value nil
  :lighter " BIG"
  :global t
  (setq-default powerline-height (if big-mode big-mode-modeline-height big-mode--powerline-height))
  (narf/load-font (if big-mode big-mode-font narf-default-font))
  (setq-default line-spacing (if big-mode big-mode-line-spacing big-mode--line-spacing)))

(provide 'module-demo)
;;; module-demo.el ends here
