;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "6ce56331445ad7ef79a11d5a3163364a883067c2")
(package! cuda-mode :pin "7f593518fd135fc6af994024bcb47986dfa502d2")
(package! demangle-mode :pin "04f545adab066708d6151f13da65aaf519f8ac4e")
(package! disaster :pin "0c13bd244cc43773af81e52ce73a55f199d58a61")
(package! modern-cpp-font-lock :pin "43c6b68ff58fccdf9deef11674a172e4eaa8455c")
(package! opencl-mode :pin "15091eff92c33ee0d1ece40eb99299ef79fee92d")

(when (package! glsl-mode :pin "9b2e5f28e489a1f73c4aed734105618ac0dc0c43")
  (when (modulep! :completion company)
    (package! company-glsl
      :recipe (:host github :repo "Kaali/company-glsl")
      :pin "404cd0694ab34971f9c01eb22126cd2e7d3f9dc4")))

(if (modulep! +lsp)
    (unless (modulep! :tools lsp +eglot)
      ;; ccls package is necessary only for lsp-mode.
      (package! ccls :pin "29d231590fad39b4d658d9262859e60669edb9b0"))
  (when (package! irony :pin "870d1576fb279bb93f776a71e65f45283c423a9e")
    (package! irony-eldoc :pin "73e79a89fad982a2ba072f2fcc1b4e41f0aa2978")
    (when (modulep! :checkers syntax)
      (package! flycheck-irony :pin "42dbecd4a865cabeb301193bb4d660e26ae3befe"))
    (when (modulep! :completion company)
      (package! company-irony :pin "b44711dfce445610c1ffaec4951c6ff3882b216a")
      (package! company-irony-c-headers :pin "72c386aeb079fb261d9ec02e39211272f76bbd97")))
  (when (package! rtags :pin "9e442e5b30b6e9bbd3eb3bbc59db33177f15d730")
    (when (modulep! :completion ivy)
      (package! ivy-rtags))
    (when (modulep! :completion helm)
      (package! helm-rtags))))
