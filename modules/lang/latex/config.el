;;; lang/latex/config.el

;; TODO Test me

(defvar +latex-bibtex-dir "~/work/writing/biblio/"
  "Where bibtex files are kept.")

(defvar +latex-bibtex-default-file "default.bib"
  "TODO")


;;
;; Plugins
;;

(@def-package auctex
  :mode ("\\.tex$" . LaTeX-mode)
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-source-correlate-start-server nil
        LaTeX-fill-break-at-separators nil)

  (@add-hook LaTeX-mode '(LaTeX-math-mode TeX-source-correlate-mode))

  (@set :company-backend 'LaTeX-mode '(company-auctex)))

(@def-package company-auctex)


(@def-package bibtex ; built-in
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20
        bibtex-completion-bibliography (list +latex-bibtex-default-file))

  (@map :map bibtex-mode-map
        "C-c \\" 'bibtex-fill-entry))


(@def-package reftex ; built-in
  :commands turn-on-reftex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-default-bibliography (list +latex-bibtex-default-file))

  (@map :map reftex-mode-map
        :leader :n ";" 'reftex-toc))


(@def-package ivy-bibtex
  :commands ivy-bibtex)

(@def-package helm-bibtex
  :commands helm-bibtex)

