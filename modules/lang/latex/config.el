;;; lang/latex/config.el -*- lexical-binding: t; -*-

(defvar +latex-indent-level-item-continuation 4
  "Custom indentation level for items in enumeration-type environments")

(defvar +latex-bibtex-file ""
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

(defvar +latex-enable-unicode-math nil
  "If non-nil, use `company-math-symbols-unicode' backend in LaTeX-mode,
enabling unicode symbols in math regions. This requires the unicode-math latex
package to be installed.")

;;
(defvar +latex--company-backends nil)


;;
;; Plugins
;;

;; sp's default rules are obnoxious, so disable them
(provide 'smartparens-latex)

(def-package! tex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook (TeX-mode . visual-line-mode)
  :config
  ;; fontify common latex commands
  (load! "+fontification")
  ;; select viewer
  (load! "+viewers")
  (setq TeX-parse-self t ;; parse on load
        TeX-auto-save t ;; parse on save
        ;; use hidden dirs for auctex files
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; don't start the emacs server when correlating sources
        TeX-source-correlate-start-server nil
        ;; automatically insert braces after sub/superscript in math mode
        TeX-electric-sub-and-superscript t)
  ;; prompt for master
  (setq-default TeX-master nil)
  ;; set-up chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  ;; tell emacs how to parse tex files
 (add-hook! 'tex-mode-hook (setq ispell-parser 'tex))
  ;; display output of latex commands in popup
  (set-popup-rule! " output\\*$" :size 15)
  ;; Do not prompt for Master files, this allows auto-insert to add templates to
  ;; .tex files
  (add-hook! 'TeX-mode-hook (remove-hook 'find-file-hook
                                         (cl-find-if #'byte-code-function-p find-file-hook)
                                         'local))
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (when (featurep! :feature spellcheck)
    (add-hook 'TeX-mode-hook #'flyspell-mode :append)))

; Fold TeX macros
(def-package! tex-fold
  :hook (TeX-mode . TeX-fold-mode))

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

;; set-up preview package
(def-package! preview
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))

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

;; Nicely indent lines that have wrapped when visual line mode is activated
(def-package! adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))

;; referencing + bibtex setup
(load! "+ref")

;;
;; Sub-modules
;;

(if (featurep! +latexmk) (load! "+latexmk"))
(if (featurep! +preview-pane) (load! "+preview-pane"))
