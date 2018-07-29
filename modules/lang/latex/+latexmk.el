;;; lang/latex/+latexmk.el -*- lexical-binding: t; -*-
;;;###if (featurep! +latexmk)

(def-package! auctex-latexmk
  :after latex
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default
  (setq-hook! LaTeX-mode TeX-command-default "LatexMk")
  :config
  ;; Add latexmk as a TeX target
  (auctex-latexmk-setup))
