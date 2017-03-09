;;; lang/latex/config.el

(defvar +latex-bibtex-dir "~/work/writing/biblio/"
  "Where bibtex files are kept.")

(defvar +latex-bibtex-default-file "default.bib"
  "TODO")


;;
;; Plugins
;;

;; Because tex-mode is built-in and AucTex has conflicting components, we need
;; to ensure that auctex gets loaded instead of tex-mode.
(load "auctex" nil t)
(load "auctex-autoloads" nil t)
(push '("\\.[tT]e[xX]\\'" . TeX-latex-mode) auto-mode-alist)

(after! tex-site
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-source-correlate-start-server nil
        LaTeX-fill-break-at-separators nil
        LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))

  (add-hook! (latex-mode LaTeX-mode) 'turn-on-auto-fill)
  (add-hook! LaTeX-mode '(LaTeX-math-mode TeX-source-correlate-mode))

  (set! :popup " output\\*$" :regexp t :size 15 :noselect t :autoclose t :autokill t)

  (map! :map LaTeX-mode-map "C-j" nil
        :leader
        :n ";" 'reftex-toc)

  (def-package! company-auctex
    :init
    (set! :company-backend 'LaTeX-mode '(company-auctex))))


(def-package! reftex ; built-in
  :commands (turn-on-reftex reftex-mode)
  :init
  (setq reftex-plug-into-AUCTeX t
        reftex-default-bibliography (list +latex-bibtex-default-file)
        reftex-toc-split-windows-fraction 0.2)

  (add-hook 'reftex-toc-mode-hook 'doom-hide-modeline-mode)
  (add-hook! (latex-mode LaTeX-mode) 'turn-on-reftex)
  :config
  (map! :map reftex-mode-map
        :leader :n ";" 'reftex-toc))


(def-package! bibtex ; built-in
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20
        bibtex-completion-bibliography (list +latex-bibtex-default-file))

  (map! :map bibtex-mode-map "C-c \\" 'bibtex-fill-entry))


(def-package! ivy-bibtex
  :commands ivy-bibtex)

(def-package! helm-bibtex
  :commands helm-bibtex)

