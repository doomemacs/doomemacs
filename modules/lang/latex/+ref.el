;;; lang/latex/+ref.el -*- lexical-binding: t; -*-

(defvar +latex-bibtex-file ""
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

(def-package! reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :init
  (setq reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3)
  :config
  ;; Get ReTeX working with biblatex
  ;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
  (setq reftex-cite-format
        '((?t . "\\textcite[]{%l}")
          (?a . "\\autocite[]{%l}")
          (?c . "\\cite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?b . "\\blockcquote[]{%l}{}")))
  (unless (string-empty-p +latex-bibtex-file)
    (setq reftex-default-bibliography (list (expand-file-name +latex-bibtex-file))))
  (map! :map reftex-mode-map
        :localleader :n ";" 'reftex-toc)
  (add-hook! 'reftex-toc-mode-hook
    (reftex-toc-rescan)
    (map! :local
          :e "j"   #'next-line
          :e "k"   #'previous-line
          :e "q"   #'kill-buffer-and-window
          :e "ESC" #'kill-buffer-and-window)))

(def-package! company-reftex
  :when (featurep! :completion company)
  :after reftex
  :config
  (set-company-backend! 'reftex-mode 'company-reftex-labels 'company-reftex-citations))

(def-package! bibtex
  :defer t
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20)
  (define-key bibtex-mode-map (kbd "C-c \\") #'bibtex-fill-entry))

(def-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :commands ivy-bibtex)

(def-package! helm-bibtex
  :when (featurep! :completion helm)
  :commands helm-bibtex)

(after! bibtex-completion
  (unless (string-empty-p +latex-bibtex-file)
    (setq bibtex-completion-bibliography (list (expand-file-name +latex-bibtex-file)))))


