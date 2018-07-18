;;; lang/latex/config.el -*- lexical-binding: t; -*-

(defvar +latex-indent-level-item-continuation 4
  "Custom indentation level for items in enumeration-type environments")

(defvar +latex-bibtex-file ""
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

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
 (add-hook! 'tex-mode-hook
           (lambda () (setq ispell-parser 'tex)))
  ;; display output of latex commands in popup
  (set-popup-rule! " output\\*$" :size 15)
  ;; Do not prompt for Master files, this allows auto-insert to add templates to
  ;; .tex files
  (add-hook! 'TeX-mode-hook (remove-hook 'find-file-hook
                                         (cl-find-if #'byte-code-function-p find-file-hook)
                                         'local))
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook! 'TeX-update-style-hook #'rainbow-delimiters-mode)
  (when (featurep! :feature spellcheck)
    (add-hook 'TeX-mode-hook #'flyspell-mode :append)))

; Fold TeX macros
(def-package! tex-fold
  :hook (TeX-mode . TeX-fold-mode))

(def-package! latex
  :defer t
  :config
  (setq LaTeX-section-hook ; Add the toc entry to the sectioning hooks.
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0)
  ;; Set custom item indentation
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex/LaTeX-indent-item))))

;; set-up preview package
(def-package! preview
  :defer t
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))

;; set-up company-auctex, but with company-math supplying the math symbols backend
(def-package! company-auctex
  :when (featurep! :completion company)
  :after latex
  :config
  (def-package! company-math
    :defer t
    ;; We can't use the `set-company-backend!' because Auctex reports its
    ;; major-mode as `latex-mode', but uses LaTeX-mode-hook for its mode, which is
    ;; not something `set-company-backend!' anticipates (and shouldn't have to!)
    :init
    (add-hook! LaTeX-mode
      (setq-local company-backends
                  (append '((company-math-symbols-latex
                             company-auctex-macros
                             company-auctex-environments))
                          company-backends)))))

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
