;;; lang/latex/+ref.el -*- lexical-binding: t; -*-

(when (stringp +latex-bibtex-file)
  (setq bibtex-completion-bibliography (list (expand-file-name +latex-bibtex-file))
        reftex-default-bibliography bibtex-completion-bibliography))


(use-package! reftex
  :hook (LaTeX-mode . reftex-mode)
  :config
  ;; set up completion for citations and references
  (set-company-backend! 'reftex-mode 'company-reftex-labels 'company-reftex-citations)
  ;; Get ReTeX working with biblatex
  ;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?p . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}"))
        reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3)
  (map! :map reftex-mode-map
        :localleader
        ";" 'reftex-toc)
  (add-hook! 'reftex-toc-mode-hook
    (reftex-toc-rescan)
    (map! :map 'local
          :e "j"   #'next-line
          :e "k"   #'previous-line
          :e "q"   #'kill-buffer-and-window
          :e "ESC" #'kill-buffer-and-window)))

;; set up mode for bib files
(after! bibtex
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20)
  (define-key bibtex-mode-map (kbd "C-c \\") #'bibtex-fill-entry))
