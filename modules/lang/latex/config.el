;;; lang/latex/config.el -*- lexical-binding: t; -*-

(defvar +latex-indent-level-item-continuation 4
  "Custom indentation level for items in enumeration-type environments")

(defvar +latex-bibtex-file nil
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

(defvar +latex-enable-unicode-math nil
  "If non-nil, use `company-math-symbols-unicode' backend in LaTeX-mode,
enabling unicode symbols in math regions. This requires the unicode-math latex
package to be installed.")

(defvar +latex-viewers `(skim zathura okular pdf-tools)
  "A list of enabled latex viewers to use, in this order. If they don't exist,
they will be ignored. Recognized viewers are skim, zathura, okular and
pdf-tools.

If no viewers are found, `latex-preview-pane' is used.")

;;
(defvar +latex--company-backends nil)


;;
;; Packages

(add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-latex-mode))


(after! tex
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
  ;; fontify common latex commands
  (load! "+fontification")
  ;; select viewer
  (load! "+viewers")
  ;; prompt for master
  (setq-default TeX-master nil)
  ;; set-up chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  ;; tell emacs how to parse tex files
  (setq-hook! 'TeX-mode-hook ispell-parser 'tex)
  ;; Enable word wrapping
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  ;; Fold TeX macros
  (add-hook 'TeX-mode-hook #'TeX-fold-mode)
  ;; display output of latex commands in popup
  (set-popup-rule! " output\\*$" :size 15)
  ;; Do not prompt for Master files, this allows auto-insert to add templates to
  ;; .tex files
  (add-hook! 'TeX-mode-hook
    ;; Necessary because it is added as an anonymous, byte-compiled function
    (remove-hook 'find-file-hook
                 (cl-find-if #'byte-code-function-p find-file-hook)
                 'local))
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  (when (featurep! :feature spellcheck)
    (add-hook 'TeX-mode-local-vars-hook #'flyspell-mode))
  ;; All these excess pairs dramatically slow down typing in latex buffers, so
  ;; we remove them. Let snippets do their job.
  (after! smartparens-latex
    (let ((modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)))
      (dolist (open '("\\left(" "\\left[" "\\left\\{" "\\left|"
                      "\\bigl(" "\\biggl(" "\\Bigl(" "\\Biggl(" "\\bigl["
                      "\\biggl[" "\\Bigl[" "\\Biggl[" "\\bigl\\{" "\\biggl\\{"
                      "\\Bigl\\{" "\\Biggl\\{"
                      "\\lfloor" "\\lceil" "\\langle"
                      "\\lVert" "\\lvert" "`"))
        (sp-local-pair modes open nil :actions :rem))
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
  ;; Set custom item indentation
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex/LaTeX-indent-item))))


(def-package! preview
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))


;; Nicely indent lines that have wrapped when visual line mode is activated
(def-package! adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))


(def-package! auctex-latexmk
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


(def-package! company-auctex
  :when (featurep! :completion company)
  :defer t
  :init
  (add-to-list '+latex--company-backends #'company-auctex-environments nil #'eq)
  (add-to-list '+latex--company-backends #'company-auctex-macros nil #'eq))

(def-package! company-math
  :when (featurep! :completion company)
  :defer t
  :init
  (add-to-list '+latex--company-backends #'+latex-symbols-company-backend nil #'eq))


;; bibtex + reftex
(load! "+ref")
