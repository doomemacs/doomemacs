;;; lang/latex/config.el -*- lexical-binding: t; -*-

(defvar +latex-indent-level-item-continuation 4
  "Custom indentation level for items in enumeration-type environments")

;; sp's default rules are obnoxious, so disable them
(provide 'smartparens-latex)

(after! tex
  ;; Set some varibles to fontify common LaTeX commands.
  (load! "+fontification")
;; Set-up viewers
  (load! "+viewers")

  (setq TeX-parse-self t    ; Enable parse on load.
        TeX-save-query nil  ; just save, don't ask
        TeX-auto-save t     ; Enable parse on save.
        ;; Use hidden directories for AUCTeX files.
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        ;; When correlating sources to rendered PDFs, don't start the emacs
        ;; server
        TeX-source-correlate-start-server nil
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; Fonts for section, subsection, etc
        font-latex-fontify-sectioning 1.15)
  (setq-default TeX-master nil)
  ;; Display the output of the latex commands in a popup.
  (set-popup-rule! " output\\*$" :size 15)
  ;; TeX Folding
  (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(after! latex
  (setq LaTeX-section-hook ; Add the toc entry to the sectioning hooks.
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0) ; item indentation.

  ;; Do not prompt for Master files, this allows auto-insert to add templates to
  ;; .tex files
  (add-hook! '(LaTeX-mode-hook TeX-mode-hook)
    (remove-hook 'find-file-hook
                 (cl-find-if #'byte-code-function-p find-file-hook)
                 'local))
  ;; Adding useful things for latex
  (add-hook! 'LaTeX-mode-hook
    #'(TeX-source-correlate-mode
       visual-line-mode))
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  (when (featurep! :feature spellcheck)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode :append))
  ;; Use chktex to search for errors in a latex file.
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")
  ;; Set a custom item indentation
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex/LaTeX-indent-item))))


(def-package! preview
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))

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
