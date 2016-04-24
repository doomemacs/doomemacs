;;; module-text.el

(use-package markdown-mode
  :mode ("\\.md$" "/README$")
  :init (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
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

        :i "M--"  'markdown-insert-hr))

(use-package markdown-toc :after markdown-mode)

(use-package reftex
  :config
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref")
        reftex-default-bibliography
        `(,(expand-file-name "phys.bib" write-mode-biblio-dir))))

(use-package helm-bibtex
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-text-indentation 20)
  (add-hook! bibtex-mode
    (local-set-key (kbd "C-c \\") 'bibtex-fill-entry)
    (setq fill-column 140))
  (add-hook! latex-mode 'turn-on-auto-fill)
  (add-hook! LaTeX-mode 'turn-on-auto-fill)

  :config
  (setq helm-bibtex-bibliography
        `(,(expand-file-name "phys.bib" write-mode-biblio-dir))

        helm-bibtex-library-path
        `(,(expand-file-name "phys-pdf" write-mode-biblio-dir))

        helm-bibtex-notes-path (expand-file-name "notes" write-mode-biblio-dir)
        helm-bibtex-notes-extension ".org"

        helm-bibtex-pdf-open-function
        (lambda (fpath) (async-start-process "open-pdf" "/usr/bin/open" nil fpath))))

(provide 'module-text)
;;; module-text.el ends here
