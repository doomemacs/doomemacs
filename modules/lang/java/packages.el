;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode)
(package! groovy-mode)

(when (featurep! +eclim)
  (package! eclim)
  (when (featurep! :completion company)
    (package! company-emacs-eclim)))

(cond ((featurep! +lsp)
       (depends-on! :tools lsp)
       (package! lsp-intellij)))
      ((featurep! +meghanada)
        (package! meghanada))
