;;; lang/latex/+viewers.el -*- lexical-binding: t; -*-

(letf! (defun prepend-to-list (list-var value &optional append)
         (set list-var (delete value (symbol-value list-var)))
         (add-to-list list-var value append))
  (dolist (viewer (reverse +latex-viewers))
    (pcase viewer
      (`skim
       (when-let
           (app-path
            (and (featurep :system 'macos)
                 (file-exists-p! (or "/Applications/Skim.app"
                                     "~/Applications/Skim.app"))))
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "Skim"))
         (add-to-list 'TeX-view-program-list
                      (list "Skim" (format "%s/Contents/SharedSupport/displayline -r -b %%n %%o %%b"
                                           app-path)))))

      (`sumatrapdf
       (when (and (featurep :system 'windows)
                  (executable-find "SumatraPDF"))
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "SumatraPDF"))))

      (`okular
       (when (executable-find "okular")
         ;; Configure Okular as viewer. Including a bug fix
         ;; (https://bugs.kde.org/show_bug.cgi?id=373855).
         (add-to-list 'TeX-view-program-list '("Okular" ("okular --noraise --unique file:%o" (mode-io-correlate "#src:%n%a"))))
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "Okular"))))

      (`zathura
       (when (executable-find "zathura")
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))))

      (`evince
       (when (executable-find "evince")
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "Evince"))))

      (`pdf-tools
       (when (modulep! :tools pdf)
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
         (when (featurep :system 'macos)
           ;; PDF Tools isn't in `TeX-view-program-list-builtin' on macs.
           (add-to-list 'TeX-view-program-list '("PDF Tools" TeX-pdf-tools-sync-view)))
         ;; Update PDF buffers after successful LaTeX runs.
         (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))))))
