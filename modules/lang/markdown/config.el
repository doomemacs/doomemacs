;;; lang/markdown/config.el -*- lexical-binding: t; -*-

(def-package! markdown-mode
  :mode ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode)
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-fontify-code-blocks-natively t
        markdown-hide-urls nil ; trigger with `markdown-toggle-url-hiding'
        markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-gfm-uppercase-checkbox t) ; for compat with org-mode

  :config
  (set-flyspell-predicate! '(markdown-mode gfm-mode)
    #'+markdown-flyspell-word-p)
  (set-lookup-handlers! '(markdown-mode gfm-mode)
    :file #'markdown-follow-thing-at-point)

  (defun +markdown|set-fill-column-and-line-spacing ()
    (setq-local line-spacing 2)
    (setq-local fill-column 80))
  (add-hook 'markdown-mode-hook #'+markdown|set-fill-column-and-line-spacing)
  (add-hook 'markdown-mode-hook #'auto-fill-mode)

  (sp-with-modes '(markdown-mode gfm-mode)
    (sp-local-pair "```" "```" :post-handlers '(:add ("||\n[i]" "RET"))))

  (map! :map markdown-mode-map
        :i "M-*" #'markdown-insert-list-item
        :i "M-b" #'markdown-insert-bold
        :i "M-i" #'markdown-insert-italic
        :i "M-`" #'+markdown/insert-del
        (:when (featurep! :feature evil +everywhere)
          :m "gj"  #'markdown-next-visible-heading
          :m "gk"  #'markdown-previous-visible-heading
          ;; TODO: Make context sensitive
          :m "]h"  #'markdown-next-visible-heading
          :m "[h"  #'markdown-previous-visible-heading
          :m "[p"  #'markdown-promote
          :m "]p"  #'markdown-demote
          :m "[l"  #'markdown-previous-link
          :m "]l"  #'markdown-next-link
          :i "M--" #'markdown-insert-hr
          :n "M-r" #'browse-url-of-file)
        (:localleader
          "o" #'markdown-open
          "b" #'markdown-preview
          (:prefix "i"
            "t" #'markdown-toc-generate-toc
            "i" #'markdown-insert-image
            "l" #'markdown-insert-link))))


(def-package! pandoc-mode
  :when (featurep! +pandoc)
  :commands pandoc-mode
  :hook (markdown-mode . conditionally-turn-on-pandoc)
  :init (setq markdown-command "pandoc --from=markdown --to=html --standalone --mathjax --highlight-style=pygments"))
