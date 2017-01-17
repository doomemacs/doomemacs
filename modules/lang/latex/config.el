;;; module-latex.el

(defvar doom-bibtex-dir "~/Dropbox/docs/biblio")

(use-package reftex
  :commands turn-on-reftex
  :init
  (setq reftex-plug-into-AUCTeX t
        reftex-ref-style-default-list '("Cleveref" "Hyperref" "Fancyref")
        reftex-default-bibliography
        `(,(expand-file-name "phys.bib" doom-bibtex-dir)))
  (add-hook! (LaTeX-mode latex-mode) 'turn-on-reftex))

(use-package helm-bibtex
  :commands helm-bibtex
  :init
  (setq TeX-auto-save t
        TeX-parse-self t
        bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20)
  (add-hook! bibtex-mode
    (local-set-key (kbd "C-c \\") 'bibtex-fill-entry)
    (setq fill-column 140))
  (add-hook! (LaTeX-mode latex-mode) 'turn-on-auto-fill)

  :config
  (setq helm-bibtex-bibliography
        (list (f-expand "phys.bib" doom-bibtex-dir))

        helm-bibtex-library-path
        (list (f-expand "phys-pdf" doom-bibtex-dir))

        helm-bibtex-notes-path (f-expand "notes.org" doom-bibtex-dir)

        helm-bibtex-pdf-open-function
        (lambda (fpath) (async-start-process "open-pdf" "/usr/bin/open" nil fpath))))

(provide 'module-latex)
;;; module-latex.el ends here
