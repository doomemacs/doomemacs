;;; lang/latex/config.el -*- lexical-binding: t; -*-

(defvar +latex-bibtex-file ""
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

(defvar +latex-bibtex-dir ""
  "Where bibtex files are kept.")

(defvar +latex-indent-level-item-continuation 8
  "Custom indentation level for items in enumeration-type environments")


(def-setting! :latex-bibtex-file (file)
  "Sets the default file RefTeX uses to search for citations."
  `(setq +latex-bibtex-file ,file))

(def-setting! :latex-bibtex-dir (dir)
  "Sets the directory where AUCTeX will search for PDFs associated to BibTeX references."
  `(setq +latex-bibtex-dir ,dir))


(def-package! tex-site
  :init
  ;; Manually load the AUCTEX autoloads. This is normally done by package-initialize,
  ;; ... which we do not use.
  (load "auctex.el" nil t t)
  (load "auctex-autoloads.el" nil t t)
  :config
  ;; Set some varibles to fontify common LaTeX commands.
  (load! +fontification)
  (setq
        ;; Enable parse on load.
        TeX-parse-self t
        ;; When running TeX, just save, don't ask
        TeX-save-query nil
        ;; Enable parse on save.
        TeX-auto-save t
        ;; Use hidden directories for AUCTeX files.
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        ;; When correlating sources to rendered PDFs, don't start the emacs server
        TeX-source-correlate-start-server nil
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; Fonts for section, subsection, etc
        font-latex-fontify-sectioning 1.15)
  (setq-default TeX-master nil)
  ;; Display the output of the latex commands in a popup.
  (set! :popup " output\\*$" '((size . 15)))

  ;; TeX Font Styling
  (def-package! tex-style
    :defer t)

  ;; TeX Folding
  (def-package! tex-fold
    :defer t
    :init
    (add-hook! 'TeX-mode-hook 'TeX-fold-mode))

  (def-package! latex
    :defer t
    :init
    (setq
     ;; Add the toc entry to the sectioning hooks.
     LaTeX-section-hook
       '(LaTeX-section-heading
         LaTeX-section-title
         LaTeX-section-toc
         LaTeX-section-section
         LaTeX-section-label)
     LaTeX-fill-break-at-separators nil
     ;; Item indentation.
     LaTeX-item-indent 0)
    :config
    (map! :map LaTeX-mode-map "C-j" nil)
    ;; Do not prompt for Master files, this allows auto-insert to add templates to .tex files
    (add-hook! '(LaTeX-mode TeX-mode) '(lambda () (remove-hook 'find-file-hooks (car find-file-hooks) 'local)))
    ;; Adding useful things for latex
    (add-hook! LaTeX-mode (LaTeX-math-mode) (TeX-source-correlate-mode)(TeX-global-PDF-mode t)
                          (TeX-PDF-mode t) (visual-line-mode +1))
    (when (featurep! :feature spellcheck)
      (add-hook! LaTeX-mode (flyspell-mode t)))
    ;; Default language setting.
    (setq ispell-dictionary "english")
    ;; Use chktex to search for errors in a latex file.
    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")
    ;; Set a custom item indentation
    (setq LaTeX-indent-environment-list
          (nconc '(("itemize" +latex/LaTeX-indent-item)
                   ("enumerate" +latex/LaTeX-indent-item)
                   ("description" +latex/LaTeX-indent-item))
                 LaTeX-indent-environment-list))))

(after! latex
  ;; Use Okular is the user says so.
  (when (featurep! +okular)
    ;; Configure Okular as viewer. Including a bug fix (https://bugs.kde.org/show_bug.cgi?id=373855)
    (add-to-list 'TeX-view-program-list
                 '("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
    (add-to-list 'TeX-view-program-selection
                 '(output-pdf "Okular"))))

(after! latex
  (when (featurep! +skim)
    (add-to-list 'TeX-view-program-list
                 '("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))
    (add-to-list 'TeX-view-program-selection
                 '(output-pdf "Skim"))))


(def-package! preview
  ;; The preview package is currently broken with the latest AUCTeX version ("11.90.2.2017-07-25)
  ;; ... and Ghostscript 9.22. It's now fixed in AUCTeX master, so we just have to wait.
  :init
  (progn
    (setq-default preview-scale 1.4
                  preview-scale-function '(lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))
  (add-hook! LaTeX-mode #'LaTeX-preview-setup))

(def-package! reftex
  :commands turn-on-reftex
  :init
  (setq reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3)
  (unless (string-empty-p +latex-bibtex-file)
    (setq reftex-default-bibliography (list (expand-file-name +latex-bibtex-file))))
  ; Get ReTeX working with biblatex
  ; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
  (setq reftex-cite-format
        '((?t . "\\textcite[]{%l}")
          (?a . "\\autocite[]{%l}")
          (?c . "\\cite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?b . "\\blockcquote[]{%l}{}")))
  (add-hook! (latex-mode LaTeX-mode) #'turn-on-reftex)
  :config
  (map! :map reftex-mode-map
        :localleader :n ";" 'reftex-toc)
  (add-hook! 'reftex-toc-mode-hook
    (reftex-toc-rescan)
    (map! :local
          :e "j"   #'next-line
          :e "k"   #'previous-line
          :e "q"   #'kill-buffer-and-window
          :e "ESC" #'kill-buffer-and-window
          "C-g"    #'reftex-toc-quit)))

(def-package! bibtex
  :defer t
  :mode ("\\.bib" . bibtex-mode)
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20
        bibtex-completion-bibliography (list (expand-file-name +latex-bibtex-file)))
  (map! :map bibtex-mode-map "C-c \\" #'bibtex-fill-entry))

(def-package! latex-preview-pane
  :when (featurep! +preview-pane)
  :commands latex-preview-pane-enable
  :init
  (setq latex-preview-pane-multifile-mode 'auctex)
  (add-hook! (latex-mode LaTeX-mode) #'latex-preview-pane-enable)
  (add-to-list 'TeX-view-program-list
                 '("preview-pane"
                   latex-preview-pane-mode))
  (add-to-list 'TeX-view-program-selection
                 '(output-pdf "preview-pane"))
  :config
  (map! :map doc-view-mode-map
        "ESC" #'delete-window
        "q"   #'delete-window
        "k" (λ! (quit-window) (delete-window))))

;; Enable latexmk only if the user explicitly says so with the module flag '+latexmk'.
(def-package! auctex-latexmk
  :when (featurep! +latexmk)
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default
  (add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "LatexMk")))
  :config
  ;; Add latexmk as a TeX target
  (auctex-latexmk-setup))

(def-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :commands ivy-bibtex
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
  (unless (string-empty-p +latex-bibtex-file)
    (setq bibtex-completion-bibliography (list (expand-file-name +latex-bibtex-file))))
  (unless (string-empty-p +latex-bibtex-dir)
    (setq bibtex-completion-library-path (list +latex-bibtex-dir)
          bibtex-completion-pdf-field "file"
          bibtex-completion-notes-path (f-expand "notes.org" +latex-bibtex-dir)
          bibtex-completion-pdf-open-function
          (lambda (fpath) (async-start-process "open-pdf" "/usr/bin/xdg-open" nil fpath)))))

(def-package! helm-bibtex
  :when (featurep! :completion helm)
  :commands helm-bibtex
  :config
  (unless (string-empty-p +latex-bibtex-file)
    (setq bibtex-completion-bibliography (list (expand-file-name +latex-bibtex-file))))
  (unless (string-empty-p +latex-bibtex-dir)
    (setq bibtex-completion-library-path (list +latex-bibtex-dir)
          bibtex-completion-pdf-field "file"
          bibtex-completion-notes-path (f-expand "notes.org" +latex-bibtex-dir)
          bibtex-completion-pdf-open-function
          (lambda (fpath) (async-start-process "open-pdf" "/usr/bin/xdg-open" nil fpath)))))

(def-package! company-auctex
  :when (featurep! :completion company)
  :commands (company-auctex-init)
  :init
  ;; We can't use the (set! :company-backend ...) because Auctex reports its
  ;; major-mode as `latex-mode', but uses LaTeX-mode-hook for its mode, which
  ;; is not anticipated by :company-backend (and shouldn't have to!)
  (add-hook! LaTeX-mode
    (make-variable-buffer-local 'company-backends)
    (company-auctex-init)))

;; Nicely indent lines that have wrapped when visual line mode is activated
(def-package! adaptive-wrap
  :commands (adaptive-wrap-prefix-mode)
  :init
  (add-hook! LaTeX-mode 'adaptive-wrap-prefix-mode)
  (setq-default adaptive-wrap-extra-indent 0))
