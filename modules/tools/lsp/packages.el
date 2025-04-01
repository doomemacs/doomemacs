;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (modulep! +eglot)
    (progn
      (package! eglot :pin "8711f483bedf01cf9bf8572a306b04b6f9c51ec0")
      (when (modulep! :completion vertico)
        (package! consult-eglot :pin "b71499f4b93bfea4e2005564c25c5bb0f9e73199"))
      (when (modulep! :checkers syntax -flymake)
        (package! flycheck-eglot :pin "18d0c9869585e6a9ea5c40678f266cf7f5bb2d2e")))
  (package! lsp-mode :pin "4cddd2d99d9e61609396e562df876351007aaf2b")
  (package! lsp-ui :pin "09d40806429fadc01a12d9a1841b49430f58adb5")
  (when (modulep! :completion ivy)
    (package! lsp-ivy :pin "bdc730a20961da4a1996298a95a7edf9ae00f2ae"))
  (when (modulep! :completion helm)
    (package! helm-lsp :pin "cf4ea6fb421e7cd0ea6e3d76e5fa2cee4cdb8bb7"))
  (when (modulep! :completion vertico)
    (package! consult-lsp :pin "aef321d03907ca6926b0cf20ca85f672c4744000")))
