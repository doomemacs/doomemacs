;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode :pin "67f7c0d7d37605efc7f055b76d731556861c3eb9")
(package! groovy-mode :pin "7b8520b2e2d3ab1d62b35c426e17ac25ed0120bb")

(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    ;; HACK: lsp-java depends on lsp-treemacs without declaring it as a
    ;;   dependency, thereby throwing errors if :ui (treemacs +lsp) isn't
    ;;   enabled (i.e. lsp-treemacs isn't installed). This needs to be tackled
    ;;   upstream, but for now:
    (unless (alist-get 'lsp-treemacs doom-packages)
      (package! lsp-treemacs :pin "3e5550f278db74f15ebe34add0138b138207ec08"))
    (package! lsp-java :pin "9230a0007c79a661028c142f35c7b8d1f9f55453")))
