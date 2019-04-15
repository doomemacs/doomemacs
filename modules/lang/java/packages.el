;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode)
(package! groovy-mode)

(when (not (featurep! :tools gtags))
  (when (featurep! +meghanada)
    (package! meghanada))

  (when (featurep! +eclim)
    (package! eclim)
    (when (featurep! :completion company)
      (package! company-emacs-eclim)))

  (when (featurep! +lsp)
    (package! lsp-java)))
