;;; module-text.el

(use-package markdown-mode
  :mode (("\\.md$"   . markdown-mode)
         ("/README$" . markdown-mode))
  :functions (markdown-use-region-p
              markdown-unwrap-things-in-region
              markdown-wrap-or-insert
              markdown-unwrap-thing-at-point)
  :init
  (add-hook! markdown-mode 'narf|enable-hard-wrap)
  :config
  (add-hook! markdown-mode
    (exmap! "preview"  'narf/markdown-preview)
    (exmap! "export"   'narf:org-export))
  (map! (:map markdown-mode-map
          "<backspace>"  nil
          "<M-left>"     nil
          "<M-right>"    nil

          ;; Assumes you have a markdown renderer plugin in chrome
          :nv "M-r"  (λ! (narf-open-with "Google Chrome"))

          "M-*"  'markdown-insert-list-item
          "M-b"  'markdown-insert-bold
          "M-i"  'markdown-insert-italic
          "M-`"  'narf/markdown-insert-del

          (:localleader
           :nv "i"   'markdown-insert-image
           :nv "l"   'markdown-insert-link
           :nv "L"   'markdown-insert-reference-link-dwim
           :nv "b"   'markdown-preview)

          ;; TODO: Make context sensitive
          :n "[p"   'markdown-promote
          :n "]p"   'markdown-demote

          :i "M--"  'markdown-insert-hr)))

(provide 'module-text)
;;; module-text.el ends here
