;;; lang/latex/+viewers.el -*- lexical-binding: t; -*-

;; Update PDF buffers after successful LaTeX runs

(when (featurep! +okular)
  ;; Configure Okular as viewer. Including a bug fix
  ;; (https://bugs.kde.org/show_bug.cgi?id=373855)
  (add-to-list 'TeX-view-program-list '("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))

;; Or Skim
(when (featurep! +skim)
  (add-to-list 'TeX-view-program-selection 'output-pdf '("Skim")))

;; Or Zathura
(when (featurep! +zathura)
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))

;; Or PDF-tools, but only if the module is also loaded
(when (and (featurep! :tools pdf)
           (featurep! +pdf-tools))
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-hook 'TeX-after-compilation-finished-function #'TeX-revert-document-buffer))
