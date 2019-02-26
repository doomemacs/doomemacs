;;; lang/latex/+viewers.el -*- lexical-binding: t; -*-

(catch 'found-viewer
  (dolist (viewer +latex-viewers)
    (if (pcase viewer
          (`skim
           (when (and IS-MAC
                      (file-exists-p! (or "/Applications/Skim.app"
                                          "~/Applications/Skim.app")))
             (add-to-list 'TeX-view-program-selection '(output-pdf "Skim"))))
          (`sumatrapdf
           (when (and IS-WINDOWS
                      (executable-find "SumatraPDF"))
             (add-to-list 'TeX-view-program-selection '(output-pdf "SumatraPDF"))))

          (`okular
           (when (executable-find "okular")
             ;; Configure Okular as viewer. Including a bug fix
             ;; (https://bugs.kde.org/show_bug.cgi?id=373855)
             (add-to-list 'TeX-view-program-list '("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
             (add-to-list 'TeX-view-program-selection '(output-pdf "Okular"))))

          (`zathura
           (when (executable-find "zathura")
             (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))))

          (`pdf-tools
           (when (featurep! :tools pdf)
             (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
             (when IS-MAC
               ;; PDF Tools isn't in `TeX-view-program-list-builtin' on macs
               (add-to-list 'TeX-view-program-list '("PDF Tools" TeX-pdf-tools-sync-view)))
             ;; Update PDF buffers after successful LaTeX runs
             (add-hook 'TeX-after-compilation-finished-function #'TeX-revert-document-buffer))))

        (throw 'found-viewer t)))

  ;; fall back to latex-preview-pane
  (add-to-list 'TeX-view-program-list '("preview-pane" latex-preview-pane-mode))
  (add-to-list 'TeX-view-program-selection '(output-pdf "preview-pane")))


(after! latex-preview-pane
  (setq latex-preview-pane-multifile-mode 'auctex)

  (define-key! doc-view-mode-map
    "ESC" #'delete-window
    "q"   #'delete-window
    "k"   (λ! (quit-window) (delete-window))))
