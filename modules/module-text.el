;;; module-text.el

(use-package markdown-mode
  :mode ("\\.md$" "/README$")
  :init
  (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
  :config
  (map! :map markdown-mode-map
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

        :i "M--"  'markdown-insert-hr)

  (use-package markdown-toc :commands (markdown-toc-generate-toc)))

(provide 'module-text)
;;; module-text.el ends here
