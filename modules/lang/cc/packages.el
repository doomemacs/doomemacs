;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "d5d77de8c4c69e348b182eeb30222b2f1ba8db7b")
(package! cuda-mode :pin "7f593518fd135fc6af994024bcb47986dfa502d2")
(package! demangle-mode :pin "aaef0bd77a3ea9ce9132e9a53ac021b0f5d33e12")
(package! disaster :pin "10a785facc60d89d78e0d5177985ab1af1741bb4")
(package! modern-cpp-font-lock :pin "43c6b68ff58fccdf9deef11674a172e4eaa8455c")
(package! opencl-mode :pin "15091eff92c33ee0d1ece40eb99299ef79fee92d")

(when (package! glsl-mode :pin "b07112016436d9634cd4ef747f9af6b01366d136")
  (when (featurep! :completion company)
    (package! company-glsl
      :recipe (:host github :repo "Kaali/company-glsl")
      :pin "404cd0694ab34971f9c01eb22126cd2e7d3f9dc4")))

(if (featurep! +lsp)
    (unless (featurep! :tools lsp +eglot)
      ;; ccls package is necessary only for lsp-mode.
      (package! ccls :pin "675a5704c14a27931e835a431beea3631d92e8e6"))
  (when (package! irony :pin "ec6dce7ee16ffaa9a735204534aa4aa074d14487")
    (package! irony-eldoc :pin "73e79a89fad982a2ba072f2fcc1b4e41f0aa2978")
    (when (featurep! :checkers syntax)
      (package! flycheck-irony :pin "42dbecd4a865cabeb301193bb4d660e26ae3befe"))
    (when (featurep! :completion company)
      (package! company-irony :pin "b44711dfce445610c1ffaec4951c6ff3882b216a")
      (package! company-irony-c-headers :pin "72c386aeb079fb261d9ec02e39211272f76bbd97")))
  (when (package! rtags :pin "63f18acb21e664fd92fbc19465f0b5df085b5e93")
    (when (featurep! :completion ivy)
      (package! ivy-rtags))
    (when (featurep! :completion helm)
      (package! helm-rtags))))
