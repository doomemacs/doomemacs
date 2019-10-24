;;; lang/latex/config.el -*- lexical-binding: t; -*-

(defvar +latex-indent-level-item-continuation 4
  "Custom indentation level for items in enumeration-type environments")

(defvar +latex-bibtex-file nil
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

(defvar +latex-enable-unicode-math nil
  "If non-nil, use `company-math-symbols-unicode' backend in LaTeX-mode,
enabling unicode symbols in math regions. This requires the unicode-math latex
package to be installed.")

(defvar +latex-viewers '(skim evince sumatrapdf zathura okular pdf-tools)
  "A list of enabled latex viewers to use, in this order. If they don't exist,
they will be ignored. Recognized viewers are skim, evince, sumatrapdf, zathura,
okular and pdf-tools.

If no viewers are found, `latex-preview-pane' is used.")

;;
(defvar +latex--company-backends nil)


;;
;; Packages

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(setq TeX-parse-self t ; parse on load
      TeX-auto-save t  ; parse on save
      ;; use hidden dirs for auctex files
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      ;; don't start the emacs server when correlating sources
      TeX-source-correlate-start-server nil
      ;; automatically insert braces after sub/superscript in math mode
      TeX-electric-sub-and-superscript t)

(after! tex
  ;; fontify common latex commands
  (load! "+fontification")
  ;; select viewer
  (load! "+viewers")
  ;; do not prompt for master
  (setq-default TeX-master t)
  ;; set-up chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  ;; tell emacs how to parse tex files
  (setq-hook! 'TeX-mode-hook ispell-parser 'tex)
  ;; Enable word wrapping
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  ;; Fold TeX macros
  (add-hook 'TeX-mode-hook #'TeX-fold-mode)
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  ;; display output of latex commands in popup
  (set-popup-rule! " output\\*$" :size 15)
  (after! smartparens-latex
    (let ((modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)))
      ;; All these excess pairs dramatically slow down typing in latex buffers,
      ;; so we remove them. Let snippets do their job.
      (dolist (open '("\\left(" "\\left[" "\\left\\{" "\\left|"
                      "\\bigl(" "\\biggl(" "\\Bigl(" "\\Biggl(" "\\bigl["
                      "\\biggl[" "\\Bigl[" "\\Biggl[" "\\bigl\\{" "\\biggl\\{"
                      "\\Bigl\\{" "\\Biggl\\{"
                      "\\lfloor" "\\lceil" "\\langle"
                      "\\lVert" "\\lvert" "`"))
        (sp-local-pair modes open nil :actions :rem))
      ;; And tweak these so that users can decide whether they want use latex
      ;; quotes or not, via `+latex-enable-plain-double-quotes'
      (sp-local-pair modes "``" nil :unless '(:add sp-in-math-p)))))


(after! latex
  (setq LaTeX-section-hook ; Add the toc entry to the sectioning hooks.
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0)
  (when +latex--company-backends
    (set-company-backend! 'latex-mode +latex--company-backends))

  ;; Provide proper indentation for LaTeX "itemize","enumerate", and
  ;; "description" environments. See
  ;; http://emacs.stackexchange.com/questions/3083/how-to-indent-items-in-latex-auctex-itemize-environments
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex/LaTeX-indent-item)))

  ;; Fix #1849: allow fill-paragraph in itemize/enumerate
  (defadvice! +latex--re-indent-itemize-and-enumerate-a (orig-fn &rest args)
    :around #'LaTeX-fill-region-as-para-do
    (let ((LaTeX-indent-environment-list
           (append LaTeX-indent-environment-list
                   '(("itemize"   +latex/LaTeX-indent-item)
                     ("enumerate" +latex/LaTeX-indent-item)))))
      (apply orig-fn args)))
  (defadvice! +latex--dont-indent-itemize-and-enumerate-a (orig-fn &rest args)
    :around #'LaTeX-fill-region-as-paragraph
    (let ((LaTeX-indent-environment-list LaTeX-indent-environment-list))
      (delq! "itemize" LaTeX-indent-environment-list 'assoc)
      (delq! "enumerate" LaTeX-indent-environment-list 'assoc)
      (apply orig-fn args))))


(use-package! preview
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))


;; Nicely indent lines that have wrapped when visual line mode is activated
(use-package! adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))


(use-package! auctex-latexmk
  :when (featurep! +latexmk)
  :after latex
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default
  (setq-hook! LaTeX-mode TeX-command-default "LatexMk")
  :config
  ;; Add latexmk as a TeX target
  (auctex-latexmk-setup))


(use-package! company-auctex
  :when (featurep! :completion company)
  :defer t
  :init
  (add-to-list '+latex--company-backends #'company-auctex-environments nil #'eq)
  (add-to-list '+latex--company-backends #'company-auctex-macros nil #'eq))

(use-package! company-math
  :when (featurep! :completion company)
  :defer t
  :init
  (add-to-list '+latex--company-backends #'+latex-symbols-company-backend nil #'eq))


;; bibtex + reftex
(load! "+ref")
