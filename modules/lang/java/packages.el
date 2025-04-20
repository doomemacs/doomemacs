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
      (package! lsp-treemacs :pin "312dee2b3ab776868c2b367d0ac15259689d981a"))
    (package! lsp-java :pin "6cfff8761e9f23889c002984f61e4ae04979eaf5")))
