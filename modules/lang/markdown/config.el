;;; lang/markdown/config.el -*- lexical-binding: t; -*-

(def-package! markdown-mode
  :mode ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode)
  :init
  (when (featurep! +pandoc)
    (setq markdown-command "pandoc --from=markdown --to=html --standalone --mathjax --highlight-style=pygments"))

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
  (defun +markdown|set-fill-column-and-line-spacing ()
    (setq-local line-spacing 2)
    (setq-local fill-column 80))
  (add-hook 'markdown-mode-hook #'+markdown|set-fill-column-and-line-spacing)
  (add-hook 'markdown-mode-hook #'auto-fill-mode)

  (define-key! markdown-mode-map
    [remap find-file-at-point] #'markdown-follow-thing-at-point
    (kbd "M-*") #'markdown-insert-list-item
    (kbd "M-b") #'markdown-insert-bold
    (kbd "M-i") #'markdown-insert-italic
    (kbd "M-`") #'+markdown/insert-del)
  (when (featurep! :feature evil +everywhere)
    (evil-define-key* 'motion markdown-mode-map
      "gj"    #'markdown-next-visible-heading
      "gk"    #'markdown-previous-visible-heading
      ;; TODO: Make context sensitive
      "]h"    #'markdown-next-visible-heading
      "[h"    #'markdown-previous-visible-heading
      "[p"    #'markdown-promote
      "]p"    #'markdown-demote
      "[l"    #'markdown-next-link
      "]l"    #'markdown-previous-link)
    (evil-define-key* 'insert markdown-mode-map
      (kbd "M--") #'markdown-insert-hr)
    (evil-define-key* 'normal markdown-mode-map
      (kbd "M-r") #'browse-url-of-file))
  (map! :map markdown-mode-map
        :localleader
        :nv "o" #'markdown-open
        :nv "b" #'markdown-preview
        (:prefix "i"
          :nv "t" #'markdown-toc-generate-toc
          :nv "i" #'markdown-insert-image
          :nv "l" #'markdown-insert-link)))

(def-package! pandoc-mode
  :when (featurep! +pandoc)
  :commands pandoc-mode
  :hook (markdown-mode . conditionally-turn-on-pandoc))
