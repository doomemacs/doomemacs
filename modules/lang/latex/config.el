;;; lang/latex/config.el -*- lexical-binding: t; -*-

(defvar +latex-bibtex-file ""
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

(defvar +latex-bibtex-dir ""
  "Where bibtex files are kept.")

(defvar +latex-indent-level-item-continuation 4
  "Custom indentation level for items in enumeration-type environments")


;;
;; Plugins
;;

;; sp's default rules are obnoxious, so disable them
(provide 'smartparens-latex)

(after! tex
  ;; Set some varibles to fontify common LaTeX commands.
  (load! "+fontification")

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

  ;; TeX Font Styling
  ;; (def-package! tex-style :defer t)

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

  (define-key LaTeX-mode-map "\C-j" nil)

  ;; Do not prompt for Master files, this allows auto-insert to add templates to
  ;; .tex files
  (add-hook! '(LaTeX-mode-hook TeX-mode-hook)
    (remove-hook 'find-file-hook
                 (cl-find-if #'byte-code-function-p find-file-hook)
                 'local))
  ;; Adding useful things for latex
  (add-hook! 'LaTeX-mode-hook
    #'(LaTeX-math-mode
       TeX-source-correlate-mode
       TeX-global-PDF-mode
       TeX-PDF-mode
       visual-line-mode))
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  (when (featurep! :feature spellcheck)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode :append))
  ;; Use chktex to search for errors in a latex file.
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")
  ;; Set a custom item indentation
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex/LaTeX-indent-item)))

  ;;
  ;; Use Okular if the user says so.
  (when (featurep! +okular)
    ;; Configure Okular as viewer. Including a bug fix
    ;; (https://bugs.kde.org/show_bug.cgi?id=373855)
    (add-to-list 'TeX-view-program-list '("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
    (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))

  ;; Or Skim
  (when (featurep! +skim)
    (add-to-list 'TeX-view-program-list '("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))
    (add-to-list 'TeX-view-program-selection 'output-pdf '("Skim")))

  ;; Or Zathura
  (when (featurep! +zathura)
    (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))

  ;; Or PDF-tools, but only if the module is also loaded
  (when (and (featurep! :tools pdf)
             (featurep! +pdf-tools))
    (add-to-list 'TeX-view-program-list '("PDF Tools" "TeX-pdf-tools-sync-view"))
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
    ;; Enable auto reverting the PDF document with PDF Tools
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)))


;; The preview package is currently broken with the latest AUCTeX version
;; ("11.90.2.2017-07-25) ... and Ghostscript 9.22. It's now fixed in AUCTeX
;; master, so we just have to wait.
(def-package! preview
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))


(def-package! latex-preview-pane
  :when (featurep! +preview-pane)
  :hook ((latex-mode LaTeX-mode) . latex-preview-pane-enable)
  :commands latex-preview-pane-mode
  :init
  (setq latex-preview-pane-multifile-mode 'auctex)
  :config
  (add-to-list 'TeX-view-program-list '("preview-pane" latex-preview-pane-mode))
  (add-to-list 'TeX-view-program-selection '(output-pdf "preview-pane"))
  (define-key! doc-view-mode-map
    (kbd "ESC") #'delete-window
    "q" #'delete-window
    "k" (λ! (quit-window) (delete-window))))


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


(def-package! bibtex
  :defer t
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20)
  (define-key bibtex-mode-map (kbd "C-c \\") #'bibtex-fill-entry))


(def-package! auctex-latexmk
  :when (featurep! +latexmk)
  :after-call (latex-mode-hook LaTeX-mode-hook)
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default
  (setq-hook! LaTeX-mode TeX-command-default "LatexMk")
  :config
  ;; Add latexmk as a TeX target
  (auctex-latexmk-setup))


(def-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :commands ivy-bibtex
  )


(def-package! helm-bibtex
  :when (featurep! :completion helm)
  :commands helm-bibtex
  )

(after! bibtex-completion
  (unless (string-empty-p +latex-bibtex-file)
    (setq bibtex-completion-bibliography (list (expand-file-name +latex-bibtex-file))))
  (unless (string-empty-p +latex-bibtex-dir)
    (setq bibtex-completion-library-path (list +latex-bibtex-dir)
          bibtex-completion-notes-path (expand-file-name "notes.org" +latex-bibtex-dir))))


(def-package! company-auctex
  :when (featurep! :completion company)
  :commands (company-auctex-init)
  :init
  ;; We can't use the `set-company-backend!' because Auctex reports its
  ;; major-mode as `latex-mode', but uses LaTeX-mode-hook for its mode, which is
  ;; not something `set-company-backend!' anticipates (and shouldn't have to!)
  (add-hook! LaTeX-mode
    (make-local-variable 'company-backends)
    (company-auctex-init)))


;; Nicely indent lines that have wrapped when visual line mode is activated
(def-package! adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))
