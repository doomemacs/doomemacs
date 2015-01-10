;; Global editor behavior
(use-package expand-region
  :commands (er/expand-region er/contract-region))

(use-package rotate-text
  :commands (rotate-word-at-point rotate-region))

(use-package smartparens
  :init (require 'smartparens-config)
  :config
  (progn
    (smartparens-global-mode 1)

    (setq blink-matching-paren t)
    (setq sp-autowrap-region nil            ; let evil-surround handle this
          sp-highlight-pair-overlay nil
          sp-show-pair-delay 0
          sp-autoescape-string-quote t)

    ;; Handle newlines
    (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-pair "[" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-with-modes '(emacs-lisp-mode lisp-mode)
      (sp-local-pair "[" nil :post-handlers '(("|" "RET"))))

    ;; Auto-close more conservatively
    (sp-pair "[" nil :unless '(sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "(" nil :unless '(sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "\"" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "'" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "\"" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))

    (after "yasnippet"
      (defadvice yas-expand (before advice-for-yas-expand activate)
        (sp-remove-active-pair-overlay)))))


(provide 'core-editor)
;;; core-editor.el ends here
