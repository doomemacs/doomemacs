;;; module-eshell.el

(use-package eshell
  :defer t
  :init
  (evil-set-initial-state 'eshell-mode 'normal)
  (add-popwin-rule! "*eshell*" :position left :width 80 :stick t :dedicated t)

  (setq eshell-directory-name (! (concat narf-temp-dir "eshell"))
        eshell-scroll-to-bottom-on-input 'all
        eshell-buffer-shorthand t

        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t

        ;; em-alias
        eshell-aliases-file (concat narf-temp-dir ".eshell-aliases"))

  ;; plan 9 smart shell
  (require 'em-smart)
  (add-to-list 'eshell-modules-list 'eshell-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)

  ;; em-prompt
  (setq eshell-prompt-function 'narf/eshell-prompt)

  (bind! :map eshell-mode-map
         :n "i" 'narf/eshell-evil-prepend-maybe
         :n "I" 'narf/eshell-evil-prepend
         :n "a" 'narf/eshell-evil-append-maybe
         :n "A" 'narf/eshell-evil-append
         :n "r" 'narf/eshell-evil-replace-maybe
         :n "R" 'narf/eshell-evil-replace-state-maybe))

(provide 'module-eshell)
;;; module-eshell.el ends here
