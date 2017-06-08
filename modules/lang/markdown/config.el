;;; lang/markdown/config.el -*- lexical-binding: t; -*-

(def-package! markdown-mode
  :mode ("\\.m\\(d\\|arkdown\\)$" "/README$"
         ("/README\\.md$" . gfm-mode))
  :init
  (setq markdown-enable-wiki-links t
        markdown-enable-math t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh"))

  :config
  (add-hook! markdown-mode
    (auto-fill-mode +1)
    (setq line-spacing 2
          fill-column 80))

  (sp-local-pair
   '(markdown-mode gfm-mode)
   "\`\`\`" "\`\`\`" :post-handlers '(("||\n" "RET")))

  (map! :map gfm-mode-map
        "`" #'self-insert-command

        :map markdown-mode-map
        "<backspace>"  nil
        "<M-left>"     nil
        "<M-right>"    nil
        "M-*"      #'markdown-insert-list-item
        "M-b"      #'markdown-insert-bold
        "M-i"      #'markdown-insert-italic
        "M-`"      #'+markdown/insert-del
        :m "gj"    #'markdown-next-visible-heading
        :m "gk"    #'markdown-previous-visible-heading
        ;; Assumes you have a markdown renderer plugin in chrome
        :n "M-r"   #'browse-url-of-file
        ;; TODO: Make context sensitive
        :n "[p"    #'markdown-promote
        :n "]p"    #'markdown-demote
        :n "[l"    #'markdown-next-link
        :n "]l"    #'markdown-previous-link
        :n "gf"    #'markdown-follow-thing-at-point
        :i "M--"   #'markdown-insert-hr

        :localleader
        :nv "o"    #'markdown-open
        :nv "b"    #'markdown-preview
        (:prefix "i"
          :nv "t"    #'markdown-toc-generate-toc
          :nv "i"    #'markdown-insert-image
          :nv "l"    #'markdown-insert-link
          :nv "L"    #'markdown-insert-reference-link-dwim)))


(def-package! markdown-toc
  :commands markdown-toc-generate-toc)

